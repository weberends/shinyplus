"""
PLUS.nl async client.

Uses Playwright for login (OAuth2 browser flow) and cart operations.
Intercepts network requests during the session to discover direct REST API
endpoints — printed at the end of each run so they can be hardcoded later
to replace browser automation entirely.
"""
from __future__ import annotations

import asyncio
import re
from typing import Optional

import httpx
from playwright.async_api import async_playwright, Browser, BrowserContext, Page

from .models import Cart, CartItem, Promotion, PromotionProduct, PromotionResult
from .api import SessionState
from . import version_cache

_PROMOTIONS_URL = (
    "https://www.plus.nl/screenservices/ECP_Composition_CW/Promotions"
    "/Promotion_LP_Content_TF_Optimization/DataActionGetPromotionList_Optimization"
)
_CHANNEL_ID = "1690b994-7511-41cc-a1bc-aacf2726f218"

# OutSystems type schema for the LocalPromotionList screen variable.
# Must be sent verbatim so the server knows the expected return type.
_PROMO_EMPTY_ITEM = {
    "ProductPromotionBanner": {
        "InternalTitle": "", "Subtitle": "", "Title": "", "AnchorLinkTitle": "",
        "Cta": {"InternalTitle": "", "Link": {"Title": "", "Url": "", "AltText": "", "IsPdf": False}},
        "BackgroundColorClassName": "",
        "BannerImageNoProducts": {"AltText": ""},
        "BannerImageWithProducts": {"AltText": ""},
        "Productspromotions": {"List": [], "EmptyListItem": ""},
        "ProductPromotionTiles": {
            "List": [],
            "EmptyListItem": {
                "PromotionId": "", "OfferId": "", "ProductName": "", "PromotionLabel": "",
                "PromotionBasedLabel": "", "Subtitle": "", "Brand": "", "Slug": "",
                "DisplayInfo_Label": "", "DisplayInfo_PromotionBasedLabel": "",
                "NewPrice": "0", "PriceOriginal": "0", "PriceOriginal_Highest": "0",
                "PriceOriginal_Lowest": "0", "StartDate": "1900-01-01", "EndDate": "1900-01-01",
                "ImageURL": "", "ImageLabel": "", "Position": 0,
                "IsProduct": False, "IsFreeDeliveryOffer": False, "IsSingleProductPromotion": False,
                "BadgeQuantity": 0,
                "Logos": {
                    "PLPInUpperLeft": {"List": [], "EmptyListItem": {"Name": "", "LongDescription": "", "URL": "", "Order": 0}},
                    "PLPAboveTitle": {"List": [], "EmptyListItem": {"Name": "", "LongDescription": "", "URL": "", "Order": 0}},
                    "PLPBehindSizeUnit": {"List": [], "EmptyListItem": {"Name": "", "LongDescription": "", "URL": "", "Order": 0}},
                },
                "IsProductOverMajorityAge": False,
                "Categories": {"List": [], "EmptyListItem": {"Name": ""}},
                "PromotionVariant": "", "PromotionPackage": "", "PromotionExplanation": "",
                "ProductSKU": "", "ProductLineItemId": "", "StampURL": "", "MaxOrderLimit": 0,
            },
        },
        "IsUnderAge": False, "ClickDelayValue": 0,
        "ProductCategories": "", "PromotionCategories": "", "Priority": 0,
        "UpdatedAt": "1900-01-01T00:00:00",
        "ProductCategoriesList": {"List": [], "EmptyListItem": ""},
        "PromotionCategoriesList": {"List": [], "EmptyListItem": ""},
        "PlacementId": "",
    },
    "Category": {
        "CategoryId": "", "CategoryLabel": "", "CategorySortOrder": "0",
        "Offers": {
            "List": [],
            "EmptyListItem": {
                "PromotionID": "", "Offer_Id": "", "PromotionSortOrder": "0",
                "Brand": "", "Name": "", "Example": "", "Variant": "", "Explanation": "",
                "Package": "", "Slug": "", "ImageURL": "", "ImageLabel": "",
                "MetaTitle": "", "MetaDescription": "",
                "NewPrice": "0", "PriceOriginal_Product": "0",
                "PriceOriginal_Highest": "0", "PriceOriginal_Lowest": "0",
                "IsOfflineSaleOnly": False, "IsProductOverMajorityAge": False,
                "DisplayInfo_Label": "", "DisplayInfo_PromotionBasedLabel": "",
                "StartDate": "1900-01-01", "EndDate": "1900-01-01",
                "IsFreeDeliveryOffer": False, "IsSingleProduct": False,
                "Product_SKU": "", "Product_LineItemId": "", "Product_Quantity": 0,
                "ProductLoyaltyInfoID": 0, "Product_IsNIX18": False, "Product_MaxOrderLimit": 0,
                "StampURL": "",
                "StoreNumberList": {"List": [], "EmptyListItem": ""},
            },
        },
        "SKUsAvailable": {"List": [], "EmptyListItem": ""},
        "NumberOfProducts": 0,
    },
}

# OAuth2 login URL extracted from the original R code (plus_remote_functions.R:33).
# client_id=web_ecop_eprod is the PLUS.nl web app identifier.
_LOGIN_URL = (
    "https://aanmelden.plus.nl/plus/login/"
    "?goto=https%3A%2F%2Faanmelden.plus.nl%2Fplus%2Fauth%2Foauth2.0%2Fv1%2Fauthorize"
    "%3Fresponse_type%3Dcode%26scope%3Dopenid%2Bprofile%26client_id%3Dweb_ecop_eprod"
    "%26redirect_uri%3Dhttps%253A%252F%252Fwww.plus.nl%252FCallback"
)
_CART_URL = "https://www.plus.nl/winkelwagen"


class PlusClient:
    """
    Async context manager for PLUS.nl.

    Usage::

        async with PlusClient(headless=False) as client:
            await client.login(email, password)
            await client.add_to_cart("957806", quantity=2)
            cart = await client.get_cart()
            print(cart)
            client.print_api_discoveries()
    """

    def __init__(self, headless: bool = True):
        self.headless = headless
        self._playwright = None
        self._browser: Optional[Browser] = None
        self._context: Optional[BrowserContext] = None
        self._page: Optional[Page] = None

        # Discovered API calls logged during the session
        self._api_calls: list[dict] = []
        self._bearer_token: Optional[str] = None

        # Session state captured from intercepted request/response bodies
        self._session = SessionState()

    # ------------------------------------------------------------------
    # Context manager
    # ------------------------------------------------------------------

    async def __aenter__(self) -> "PlusClient":
        self._playwright = await async_playwright().start()
        self._browser = await self._playwright.chromium.launch(headless=self.headless)
        self._context = await self._browser.new_context(
            user_agent=(
                "Mozilla/5.0 (X11; Linux x86_64) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/124.0.0.0 Safari/537.36"
            ),
            locale="nl-NL",
            timezone_id="Europe/Amsterdam",
        )
        self._page = await self._context.new_page()
        self._page.on("request", self._on_request)
        self._page.on("response", self._on_response)
        return self

    async def __aexit__(self, *_) -> None:
        if self._browser:
            await self._browser.close()
        if self._playwright:
            await self._playwright.stop()

    # ------------------------------------------------------------------
    # Network interception — discovers direct API endpoints for future use
    # ------------------------------------------------------------------

    def _on_request(self, request) -> None:
        url = request.url
        if not self._is_api_call(url):
            return
        entry = {"method": request.method, "url": url}

        auth = request.headers.get("authorization", "")
        if auth.startswith("Bearer ") and not self._bearer_token:
            self._bearer_token = auth[7:]
            entry["bearer_captured"] = True

        # Harvest version info and session fields from OutSystems screenservices bodies.
        # Each OS module has its own moduleVersion/apiVersion — capture per-module.
        if "screenservices" in url and request.method == "POST":
            try:
                body = request.post_data_json
                if isinstance(body, dict):
                    vi = body.get("versionInfo", {})
                    mv = vi.get("moduleVersion", "")
                    av = vi.get("apiVersion", "")
                    if mv:
                        # Always keep a generic fallback
                        if not self._session.module_version:
                            self._session.module_version = mv
                            self._session.api_version = av
                        # Capture module-specific versions
                        if "ECP_Cart_CW" in url and not self._session.cart_module_version:
                            self._session.cart_module_version = mv
                            self._session.cart_api_version = av
                        # Each ECP_Cart_CW action has its own apiVersion hash.
                        # When newly discovered, persist to disk so future sessions skip priming.
                        if "ActionCheckoutItem_Add" in url and not self._session.cart_add_api_version:
                            self._session.cart_add_api_version = av
                            version_cache.save_from_session(self._session)
                        if "DataActionGetCartById" in url and not self._session.cart_get_api_version:
                            self._session.cart_get_api_version = av
                        if "ActionCheckoutItem_Remove" in url and \
                                not self._session.cart_remove_api_version:
                            self._session.cart_remove_api_version = av
                            version_cache.save_from_session(self._session)
                        if "DataActionGetPromotionList_Optimization" in url and \
                                not self._session.promotions_api_version:
                            self._session.promotions_api_version = av
                            version_cache.save_from_session(self._session)
                        if "DataActionPromotionOfferDetail_Get" in url and \
                                not self._session.promo_detail_api_version:
                            self._session.promo_detail_api_version = av
                            version_cache.save_from_session(self._session)
                    params = body.get("inputParameters", {})
                    if params.get("CheckoutId") and not self._session.checkout_id:
                        self._session.checkout_id = params["CheckoutId"]
                        self._session.checkout_version = params.get("CheckoutVersion", 0)
                    if params.get("OneWelcomeUserId") and not self._session.onewelcome_user_id:
                        self._session.onewelcome_user_id = params["OneWelcomeUserId"]
                    # Harvest LineItemId↔SKU mappings from add/remove request bodies
                    if params.get("SKU") and params.get("LineItemId"):
                        self._session.line_item_ids[params["SKU"]] = params["LineItemId"]
            except Exception:
                pass

        self._api_calls.append(entry)

    def _on_response(self, response) -> None:
        url = response.url
        if not self._is_api_call(url):
            return
        for entry in reversed(self._api_calls):
            if entry["url"] == url and "status" not in entry:
                entry["status"] = response.status
                break
        # Parse any cart-mutating response to keep line_item_ids in sync.
        # Fires for both page-load calls (DataActionGetCartById) and browser-based
        # button clicks (Add/Remove), covering the prime methods automatically.
        if response.status == 200 and any(k in url for k in (
            "DataActionGetCartById", "ActionCheckoutItem_Add", "ActionCheckoutItem_Remove"
        )):
            asyncio.ensure_future(self._parse_cart_response(response))
        if response.status == 200 and "ActionStoreWrapper_GetGeneralDetails" in url \
                and not self._session.store_number:
            asyncio.ensure_future(self._parse_store_response(response))
        if response.status == 200 and "ActionCustomerTemp_GetDetails" in url \
                and not self._session.user_store_id:
            asyncio.ensure_future(self._parse_customer_response(response))

    async def _parse_cart_response(self, response) -> None:
        try:
            body = await response.json()
            data = body.get("data", {})
            checkout = (data.get("Checkout") or data.get("Cart") or
                        next(iter(data.values()), {}))
            if isinstance(checkout, dict):
                self._update_line_item_ids(checkout)
        except Exception:
            pass

    async def _parse_store_response(self, response) -> None:
        try:
            body = await response.json()
            store = body.get("data", {}).get("Store", {})
            if store.get("Store_Number"):
                self._session.store_number = int(store["Store_Number"])
        except Exception:
            pass

    async def _parse_customer_response(self, response) -> None:
        try:
            body = await response.json()
            details = body.get("data", {}).get("CustomerTempDetailsR", {})
            if details.get("PreferredStoreId"):
                self._session.user_store_id = str(details["PreferredStoreId"])
        except Exception:
            pass

    @staticmethod
    def _is_api_call(url: str) -> bool:
        # Exclude login/auth server and static assets
        if "aanmelden.plus.nl" in url:
            return False
        ext = url.split("?")[0].rsplit(".", 1)[-1].lower()
        if ext in ("js", "css", "png", "jpg", "jpeg", "webp", "svg", "ico", "woff", "woff2", "ttf"):
            return False
        # Capture all XHR/fetch calls to plus.nl — OutSystems apps use non-standard paths
        return "plus.nl" in url and any(
            kw in url
            for kw in ["/api/", "/rest/", "/service", "/screenservice", "/action/",
                       "cart", "basket", "order", "product", "search", "screenservices"]
        )

    async def add_to_cart_api(self, sku: str, quantity: int = 1) -> dict:
        """
        Add to cart via a direct fetch() call executed inside the page.

        Running the fetch from inside the browser page means it carries the
        correct Origin, Referer, and Sec-Fetch-* headers automatically — the
        same as a real XHR from plus.nl's own JavaScript. No page navigation,
        no button clicks; just one HTTP round-trip (~200ms).

        Requires the page to currently be on a plus.nl domain (same-origin).
        """
        import time
        payload = {
            "versionInfo": self._session.version_info_for_add(),
            "viewName": "MainFlow.SearchPage",
            "inputParameters": {
                "IsOrderEditMode": False,
                "CheckoutId": self._session.checkout_id,
                "CheckoutVersion": self._session.checkout_version,
                "OrderEditId": "",
                "SKU": sku,
                "QuantityToAdd": quantity,
                "ChannelId": "1690b994-7511-41cc-a1bc-aacf2726f218",
                "OneWelcomeUserId": self._session.onewelcome_user_id,
            },
        }

        t0 = time.perf_counter()
        result = await self._page.evaluate(
            """async (payload) => {
                // OutSystems CSRF: extract the 'crf=' field from the nr2Users cookie
                // and send it as the x-csrftoken header — exactly what OS's own JS does.
                const cookieMap = {};
                document.cookie.split('; ').forEach(c => {
                    const eq = c.indexOf('=');
                    if (eq > 0) cookieMap[c.slice(0, eq)] = c.slice(eq + 1);
                });
                const nr2 = decodeURIComponent(cookieMap['nr2Users'] || '');
                const crfField = nr2.split(';').find(p => p.trim().startsWith('crf=')) || '';
                const csrfToken = crfField.slice(crfField.indexOf('=') + 1);

                const resp = await fetch(
                    'https://www.plus.nl/screenservices/ECP_Cart_CW/ActionCheckoutItem_Add',
                    {
                        method: 'POST',
                        headers: {
                            'Content-Type': 'application/json',
                            'x-csrftoken': csrfToken,
                            'outsystems-locale': 'nl-NL'
                        },
                        body: JSON.stringify(payload),
                        credentials: 'include'
                    }
                );
                if (!resp.ok) {
                    const body = await resp.text().catch(() => '');
                    throw new Error('HTTP ' + resp.status + ' — ' + body.slice(0, 300));
                }
                return await resp.json();
            }""",
            payload,
        )
        elapsed = time.perf_counter() - t0
        print(f"[API] ActionCheckoutItem_Add → 200 in {elapsed * 1000:.0f}ms")

        data = result.get("data", {})
        if "Checkout" not in data:
            import json as _json_mod
            vi = result.get("versionInfo", {})
            if vi.get("hasApiVersionChanged"):
                # PLUS.nl deployed — wipe cached versions so next session re-discovers
                self._session.cart_add_api_version = ""
                self._session.cart_remove_api_version = ""
                version_cache.save_from_session(self._session)
                raise RuntimeError(
                    "PLUS.nl heeft een nieuwe versie uitgerold (hasApiVersionChanged). "
                    "Cache gewist — herstart de sessie om opnieuw te primen."
                )
            print(f"[API] Unexpected response: {_json_mod.dumps(result)[:500]}")
            raise RuntimeError("Geen 'Checkout' in response — zie debug output hierboven")

        checkout = data["Checkout"]
        self._session.checkout_version = checkout["Version"]
        self._update_line_item_ids(checkout)
        return checkout

    # ------------------------------------------------------------------
    # Remove from cart — direct API
    # ------------------------------------------------------------------

    def _update_line_item_ids(self, checkout: dict) -> None:
        """Refresh SKU→LineItemId map from any checkout response."""
        for item in checkout.get("LineItemList", {}).get("List", []):
            sku = item.get("SKU", "")
            lid = item.get("LineItemId", "")
            if sku and lid:
                self._session.line_item_ids[sku] = lid

    # ------------------------------------------------------------------
    # Promotions — direct API
    # ------------------------------------------------------------------

    async def get_promotions_api(self, next_week: bool = False) -> "PromotionResult":
        """
        Fetch current (or next week's) PLUS promotions via a direct API call.

        Uses page.evaluate(fetch(...)) so it inherits all browser cookies and
        security headers automatically. Requires store_number, user_store_id, and
        promotions_api_version — all populated automatically during login/cart
        navigation; the first call navigates to /aanbiedingen to prime them.

        Returns a PromotionResult with one Promotion per offer across all categories,
        excluding free-delivery deals.
        """
        import time as _time

        if not self._session.promotions_api_version:
            # Navigate to promotions page once to intercept versionInfo + store data
            print("[*] promotions apiVersion onbekend — navigeer naar /aanbiedingen…")
            await self._page.goto("https://www.plus.nl/aanbiedingen")
            await self._page.wait_for_load_state("networkidle", timeout=25_000)
            await asyncio.sleep(0)  # let ensure_future tasks complete

        payload = self._build_promotions_payload(next_week=next_week)

        t0 = _time.perf_counter()
        result = await self._page.evaluate(
            """async (payload) => {
                const cookieMap = {};
                document.cookie.split('; ').forEach(c => {
                    const eq = c.indexOf('=');
                    if (eq > 0) cookieMap[c.slice(0, eq)] = c.slice(eq + 1);
                });
                const nr2 = decodeURIComponent(cookieMap['nr2Users'] || '');
                const crfField = nr2.split(';').find(p => p.trim().startsWith('crf=')) || '';
                const csrfToken = crfField.slice(crfField.indexOf('=') + 1);

                const resp = await fetch(
                    'https://www.plus.nl/screenservices/ECP_Composition_CW/Promotions' +
                    '/Promotion_LP_Content_TF_Optimization/DataActionGetPromotionList_Optimization',
                    {
                        method: 'POST',
                        headers: {
                            'Content-Type': 'application/json',
                            'x-csrftoken': csrfToken,
                            'outsystems-locale': 'nl-NL'
                        },
                        body: JSON.stringify(payload),
                        credentials: 'include'
                    }
                );
                if (!resp.ok) {
                    const body = await resp.text().catch(() => '');
                    throw new Error('HTTP ' + resp.status + ' — ' + body.slice(0, 300));
                }
                return await resp.json();
            }""",
            payload,
        )
        elapsed = _time.perf_counter() - t0
        label = "volgende week" if next_week else "huidige week"
        print(f"[API] DataActionGetPromotionList_Optimization ({label}) → 200 in {elapsed * 1000:.0f}ms")

        vi = result.get("versionInfo", {})
        if vi.get("hasApiVersionChanged"):
            self._session.promotions_api_version = ""
            version_cache.save_from_session(self._session)
            raise RuntimeError(
                "PLUS.nl heeft een nieuwe versie uitgerold (hasApiVersionChanged). "
                "Cache gewist — herstart de sessie om opnieuw te primen."
            )

        return _parse_promotion_result(result["data"])

    async def get_promotion_products_api(self, slug: str) -> list["PromotionProduct"]:
        """
        Fetch individual products for a group promotion by its slug (e.g. '4431-96').

        Only meaningful for promotions where is_single_product is False.
        Returns an empty list for free-delivery offers (no products to show).
        ~400ms per call.
        """
        import time as _time

        if not self._session.promo_detail_api_version:
            # Navigate to any promo detail page once to prime the apiVersion
            print(f"[*] promo_detail apiVersion onbekend — navigeer naar /aanbiedingen/{slug}…")
            await self._page.goto(f"https://www.plus.nl/aanbiedingen/{slug}")
            await self._page.wait_for_load_state("networkidle", timeout=25_000)
            await asyncio.sleep(0)

        payload = {
            "versionInfo": self._session.version_info_for_promo_detail(),
            "viewName": "MainFlow.Promotions",
            "screenData": {
                "variables": {
                    "CheckoutId": self._session.checkout_id,
                    "IsOrderEditMode": False,
                    "OrderEditId": "",
                    "StoreChannelD": _CHANNEL_ID,
                    "LineItemRecList": {"List": [
                        {"LineItemId": lid, "SKU": sku, "Quantity": 0}
                        for sku, lid in self._session.line_item_ids.items()
                    ]},
                    "StoreNumber": self._session.store_number,
                    "PromotionOfferId": slug,
                    "_promotionOfferIdInDataFetchStatus": 1,
                    "OneWelcomeUserId": self._session.onewelcome_user_id,
                    "_oneWelcomeUserIdInDataFetchStatus": 1,
                    "IsDesktop": True,
                    "_isDesktopInDataFetchStatus": 1,
                    "IsTablet": False,
                    "_isTabletInDataFetchStatus": 1,
                    "IsPhone": False,
                    "_isPhoneInDataFetchStatus": 1,
                    "IsCustomerUnderAge": False,
                    "_isCustomerUnderAgeInDataFetchStatus": 1,
                    "IsTimetraveler": False,
                    "_isTimetravelerInDataFetchStatus": 1,
                }
            },
        }

        t0 = _time.perf_counter()
        result = await self._page.evaluate(
            """async (payload) => {
                const cookieMap = {};
                document.cookie.split('; ').forEach(c => {
                    const eq = c.indexOf('=');
                    if (eq > 0) cookieMap[c.slice(0, eq)] = c.slice(eq + 1);
                });
                const nr2 = decodeURIComponent(cookieMap['nr2Users'] || '');
                const crfField = nr2.split(';').find(p => p.trim().startsWith('crf=')) || '';
                const csrfToken = crfField.slice(crfField.indexOf('=') + 1);

                const resp = await fetch(
                    'https://www.plus.nl/screenservices/ECP_Promotion_CW/PromotionDetailsFlow' +
                    '/PromotionOffer_DP_Content/DataActionPromotionOfferDetail_Get',
                    {
                        method: 'POST',
                        headers: {
                            'Content-Type': 'application/json',
                            'x-csrftoken': csrfToken,
                            'outsystems-locale': 'nl-NL'
                        },
                        body: JSON.stringify(payload),
                        credentials: 'include'
                    }
                );
                if (!resp.ok) {
                    const body = await resp.text().catch(() => '');
                    throw new Error('HTTP ' + resp.status + ' — ' + body.slice(0, 300));
                }
                return await resp.json();
            }""",
            payload,
        )
        elapsed = _time.perf_counter() - t0
        print(f"[API] DataActionPromotionOfferDetail_Get ({slug}) → 200 in {elapsed * 1000:.0f}ms")

        vi = result.get("versionInfo", {})
        if vi.get("hasApiVersionChanged"):
            self._session.promo_detail_api_version = ""
            version_cache.save_from_session(self._session)
            raise RuntimeError(
                "PLUS.nl heeft een nieuwe versie uitgerold (hasApiVersionChanged). "
                "Cache gewist — herstart de sessie om opnieuw te primen."
            )

        return _parse_promotion_products(result["data"])

    def _build_promotions_payload(self, next_week: bool = False) -> dict:
        cart_items = [
            {"LineItemId": lid, "SKU": sku, "Quantity": 0}
            for sku, lid in self._session.line_item_ids.items()
        ]
        return {
            "versionInfo": self._session.version_info_for_promotions(),
            "viewName": "MainFlow.Promotions",
            "screenData": {
                "variables": {
                    "IsShowData": False,
                    "IsPreloadedHTMLActive": False,
                    "StoreNumber": self._session.store_number,
                    "StoreChannel": _CHANNEL_ID,
                    "PromotionPeriodId": 1,
                    "LocalPromotionList": {"List": [], "EmptyListItem": _PROMO_EMPTY_ITEM},
                    "ItemExistsInCart": {"List": cart_items},
                    "IsAppedingRecords": False,
                    "StartIndex": 0,
                    "MaxRecords": 1,
                    "IsDesktop": True,
                    "_isDesktopInDataFetchStatus": 1,
                    "IsTablet": False,
                    "_isTabletInDataFetchStatus": 1,
                    "IsPhone": False,
                    "_isPhoneInDataFetchStatus": 1,
                    "OneWelcomeUserId": self._session.onewelcome_user_id,
                    "_oneWelcomeUserIdInDataFetchStatus": 1,
                    "IsCustomerUnderAge": False,
                    "_isCustomerUnderAgeInDataFetchStatus": 1,
                    "UserStoreId": self._session.user_store_id,
                    "_userStoreIdInDataFetchStatus": 1,
                    "IsTimetraveler": False,
                    "_isTimetravelerInDataFetchStatus": 1,
                    "IsNextWeekPromotions": next_week,
                    "_isNextWeekPromotionsInDataFetchStatus": 1,
                }
            },
        }

    async def prime_remove_api(self) -> bool:
        """
        Navigate to cart, open the quantity slider on the first item, click
        the remove button once. This captures:
        - cart_remove_api_version (from the intercepted request)
        - LineItemIds for every item in the cart (from the request body)

        The first item loses 1 unit. Returns True if apiVersion was captured.
        """
        await self._page.goto(_CART_URL)
        await self._page.wait_for_load_state("networkidle", timeout=15_000)
        await self._decline_cookies_if_present()

        # Click quantity badge to open the ±-slider
        await self._page.evaluate(
            "document.querySelector('a.gtm-quantity-clicked')?.click()"
        )
        await asyncio.sleep(0.8)

        # Click remove (decrements first item by 1)
        await self._page.evaluate(
            "document.querySelector('button.gtm-remove-from-cart')?.click()"
        )

        # Poll until _on_request captures the version (max 5 s)
        deadline = asyncio.get_event_loop().time() + 5
        while not self._session.cart_remove_api_version and \
                asyncio.get_event_loop().time() < deadline:
            await asyncio.sleep(0.2)

        return bool(self._session.cart_remove_api_version)

    async def remove_from_cart_api(self, sku: str, quantity: int = 1) -> dict:
        """
        Remove `quantity` units of `sku` from the cart via direct API call.

        Requires cart_remove_api_version and a known LineItemId for this SKU.
        Call prime_remove_api() first if either is missing, or ensure
        add_to_cart_api() was called for this SKU (which populates line_item_ids).
        """
        import time as _time, json as _json

        if not self._session.cart_remove_api_version:
            print("[*] remove apiVersion onbekend — prime_remove_api() uitvoeren…")
            await self.prime_remove_api()

        line_item_id = self._session.line_item_ids.get(sku)
        if not line_item_id:
            raise ValueError(
                f"LineItemId voor SKU {sku} onbekend. "
                "Roep prime_remove_api() aan of voeg het item toe via add_to_cart_api()."
            )

        payload = {
            "versionInfo": self._session.version_info_for_remove(),
            "viewName": "MainFlow.Cart",
            "inputParameters": {
                "IsOrderEditMode": False,
                "CheckoutId": self._session.checkout_id,
                "CheckoutVersion": self._session.checkout_version,
                "OrderEditId": "",
                "LineItemId": line_item_id,
                "QuantityToRemove": quantity,
                "SKU": sku,
                "ChannelId": "1690b994-7511-41cc-a1bc-aacf2726f218",
                "OneWelcomeUserId": self._session.onewelcome_user_id,
            },
        }

        t0 = _time.perf_counter()
        result = await self._page.evaluate(
            """async (payload) => {
                const cookieMap = {};
                document.cookie.split('; ').forEach(c => {
                    const eq = c.indexOf('=');
                    if (eq > 0) cookieMap[c.slice(0, eq)] = c.slice(eq + 1);
                });
                const nr2 = decodeURIComponent(cookieMap['nr2Users'] || '');
                const crfField = nr2.split(';').find(p => p.trim().startsWith('crf=')) || '';
                const csrfToken = crfField.slice(crfField.indexOf('=') + 1);

                const resp = await fetch(
                    'https://www.plus.nl/screenservices/ECP_Cart_CW/ActionCheckoutItem_Remove',
                    {
                        method: 'POST',
                        headers: {
                            'Content-Type': 'application/json',
                            'x-csrftoken': csrfToken,
                            'outsystems-locale': 'nl-NL'
                        },
                        body: JSON.stringify(payload),
                        credentials: 'include'
                    }
                );
                if (!resp.ok) {
                    const body = await resp.text().catch(() => '');
                    throw new Error('HTTP ' + resp.status + ' — ' + body.slice(0, 300));
                }
                return JSON.stringify(await resp.json());
            }""",
            payload,
        )
        elapsed = _time.perf_counter() - t0
        print(f"[API] ActionCheckoutItem_Remove → 200 in {elapsed * 1000:.0f}ms")

        data = _json.loads(result)
        checkout = data["data"]["Checkout"]
        self._session.checkout_version = checkout["Version"]
        self._update_line_item_ids(checkout)
        return checkout

    async def get_session_state(self) -> SessionState:
        """
        Return the session state captured from intercepted requests.
        Call this after login — the page's automatic screenservices calls
        will have populated module_version, checkout_id, etc.
        If checkout_id is still missing, navigates to the cart page to trigger it.
        """
        if not self._session.checkout_id:
            # Cart page load triggers DataActionGetCartById which carries CheckoutId
            await self._page.goto(_CART_URL)
            await self._page.wait_for_load_state("networkidle", timeout=15_000)

        # Load cached apiVersions — avoids browser primes on subsequent sessions
        version_cache.apply_to_session(self._session)

        # Give pending _parse_cart_response futures a chance to complete
        # (they're scheduled with ensure_future from the response event handler)
        await asyncio.sleep(0)

        self._session.cookies = await self.get_cookies_as_dict()

        cart_mv = self._session.cart_module_version or "(not yet captured)"
        if self._session.ready:
            print(f"[+] Session state captured: checkout_id={self._session.checkout_id[:8]}… "
                  f"version={self._session.checkout_version} "
                  f"cart_module_version={cart_mv[:12]}…")
        else:
            missing = [k for k in ("checkout_id", "onewelcome_user_id")
                       if not getattr(self._session, k)]
            if not (self._session.cart_module_version or self._session.module_version):
                missing.append("module_version")
            print(f"[~] Session state incomplete, missing: {missing}")

        return self._session

    def print_api_discoveries(self) -> None:
        """Print all API calls captured during this session — useful for hardcoding endpoints."""
        if not self._api_calls:
            print("\n[API] No internal API calls captured during this session.")
            return
        seen: set[str] = set()
        unique = []
        for c in self._api_calls:
            key = f"{c['method']} {c['url'].split('?')[0]}"
            if key not in seen:
                seen.add(key)
                unique.append(c)
        print(f"\n[API] Discovered {len(unique)} unique API endpoint(s):")
        for c in unique:
            status = c.get("status", "?")
            bearer = "  [Bearer captured]" if c.get("bearer_captured") else ""
            print(f"  {c['method']:6} {status}  {c['url']}{bearer}")
        if self._bearer_token:
            print(f"\n[API] Bearer token (first 40 chars): {self._bearer_token[:40]}…")

    # ------------------------------------------------------------------
    # Cookie consent dialog
    # ------------------------------------------------------------------

    async def _decline_cookies_if_present(self) -> None:
        """Dismiss the PLUS.nl cookie wall if it is blocking the page."""
        try:
            dialog = await self._page.query_selector('[aria-label="De cookies van PLUS"]')
            if not dialog:
                return
            # Click "Weigeren" (decline) — same as the R decline_cookies() helper
            await self._page.get_by_text("Weigeren", exact=True).click(timeout=4_000)
            await self._page.wait_for_selector(
                '[aria-label="De cookies van PLUS"]', state="hidden", timeout=5_000
            )
            print("[*] Cookie dialog weggeklikt")
        except Exception:
            pass  # dialog not present or already gone

    # ------------------------------------------------------------------
    # Login
    # ------------------------------------------------------------------

    async def login(self, email: str, password: str) -> bool:
        """Login to PLUS.nl via the OAuth2 browser flow."""
        print(f"[*] Navigating to login page…")
        await self._page.goto(_LOGIN_URL)

        # If already logged in the session cookies may redirect us straight to plus.nl
        if "plus.nl" in self._page.url and "aanmelden" not in self._page.url:
            print(f"[+] Already logged in (session cookie active)")
            return True

        # Wait for login form fields
        try:
            await self._page.wait_for_selector("#username", timeout=12_000)
        except Exception:
            print("[-] Login form not found — PLUS.nl may have changed their login page")
            return False

        await self._page.fill("#username", email)
        await self._page.fill("#password", password)
        print(f"[*] Submitting credentials for {email}…")
        await self._page.click("#loginFormUsernameAndPasswordButton")

        # Wait for redirect back to plus.nl
        try:
            await self._page.wait_for_url("https://www.plus.nl/**", timeout=20_000)
        except Exception:
            print("[-] Did not redirect to plus.nl — login may have failed")
            return False

        # Dismiss cookie dialog that appears on the first post-login page
        await self._decline_cookies_if_present()

        # Confirm logged-in indicator appears
        try:
            await self._page.wait_for_selector(
                ".gtm-account-options .popover-top-label span",
                timeout=10_000,
            )
            account_el = await self._page.query_selector(
                ".gtm-account-options .popover-top-label span"
            )
            name = (await account_el.inner_text()).strip() if account_el else email
            print(f"[+] Logged in as: {name}")
            return True
        except Exception:
            # The selector might have changed; check URL as fallback
            if "plus.nl" in self._page.url and "aanmelden" not in self._page.url:
                print(f"[+] Logged in (redirected to {self._page.url})")
                return True
            print("[-] Login appeared to succeed but could not confirm account indicator")
            return False

    # ------------------------------------------------------------------
    # Add to cart
    # ------------------------------------------------------------------

    async def add_to_cart(self, sku: str, quantity: int = 1) -> bool:
        """
        Add `quantity` units of `sku` to the PLUS.nl cart.

        Uses the search page so any valid SKU works without knowing the
        full product URL slug.  Playwright waits for actual network idle
        between clicks, which is faster and more reliable than the R
        implementation's fixed `Sys.sleep(2)` per click.
        """
        search_url = f"https://www.plus.nl/zoekresultaten?SearchTerm={sku}"
        print(f"[*] Navigating to search page for SKU {sku}…")
        await self._page.goto(search_url)

        try:
            await self._page.wait_for_selector("button.gtm-add-to-cart", timeout=15_000)
        except Exception:
            print(f"[-] Add-to-cart button not found for SKU {sku}")
            return False

        # Cookie dialog may reappear on search result pages
        await self._decline_cookies_if_present()
        # Let the page settle after any overlay animation
        await self._page.wait_for_load_state("networkidle", timeout=8_000)

        # Read cart count before adding
        before = await self._cart_badge_count()

        for i in range(quantity):
            # Use JavaScript click — same as the R code's Runtime.evaluate("...click()").
            # Playwright's native click() checks for intercepting overlays (quantity wrapper
            # divs, sticky header) and times out; JS click fires directly on the element.
            clicked = await self._page.evaluate(
                "document.querySelector('button.gtm-add-to-cart')?.click(); "
                "!!document.querySelector('button.gtm-add-to-cart')"
            )
            if not clicked:
                print(f"[-] Add-to-cart button disappeared during click {i + 1}")
                break

            # Wait for badge count to increase (the cart API call completed)
            try:
                await self._page.wait_for_function(
                    f"() => {{"
                    f"  const badge = document.querySelector('.cart-badge-link .badge');"
                    f"  return badge && parseInt(badge.textContent.trim() || '0') >= {before + i + 1};"
                    f"}}",
                    timeout=10_000,
                )
            except Exception:
                # Badge selector may differ or cart was empty before; wait for network idle
                await self._page.wait_for_load_state("networkidle", timeout=8_000)

        after = await self._cart_badge_count()
        added = after - before
        if added == quantity:
            print(f"[+] Added {added}/{quantity} × SKU {sku} to cart")
            return True
        else:
            print(f"[~] Added {added}/{quantity} × SKU {sku} (badge count mismatch, check cart)")
            return added > 0

    async def _cart_badge_count(self) -> int:
        el = await self._page.query_selector(".cart-badge-link .badge")
        if not el:
            return 0
        text = (await el.inner_text()).strip()
        try:
            return int(text)
        except ValueError:
            return 0

    # ------------------------------------------------------------------
    # Get cart
    # ------------------------------------------------------------------

    async def get_cart(self) -> Cart:
        """Fetch and parse the current PLUS.nl cart."""
        print(f"[*] Loading cart page…")
        await self._page.goto(_CART_URL)
        await self._page.wait_for_load_state("networkidle", timeout=15_000)

        # Confirm the cart page has loaded
        try:
            await self._page.wait_for_selector(".cart-title-wrapper h1", timeout=10_000)
        except Exception:
            pass  # Page may still be parseable

        items_els = await self._page.query_selector_all(".cart-item-wrapper")
        items: list[CartItem] = []

        for el in items_els:
            name_el = await el.query_selector(".cart-item-name span")
            unit_el = await el.query_selector(".cart-item-complementary span")
            price_el = await el.query_selector(".cart-item-price span")
            qty_el = await el.query_selector(".cart-item-quantity span")

            if not name_el:
                continue

            name = (await name_el.inner_text()).strip()
            unit = (await unit_el.inner_text()).strip() if unit_el else ""
            qty_text = (await qty_el.inner_text()).strip() if qty_el else "1"
            price_text = (await price_el.inner_text()).strip() if price_el else "0"

            price = _parse_price(price_text)
            qty = _parse_int(qty_text)

            if name:
                items.append(CartItem(product=name, unit=unit, price=price, quantity=qty))

        # Parse final total
        total_el = await self._page.query_selector(".total-receipt-item")
        total_text = (await total_el.inner_text()).strip() if total_el else "0"
        final_total = _parse_price(total_text)

        return Cart(items=items, final_total=final_total)

    # ------------------------------------------------------------------
    # Helpers for cookie-based httpx sessions (future direct API use)
    # ------------------------------------------------------------------

    async def get_cookies_as_dict(self) -> dict[str, str]:
        """Return current browser cookies — can be passed to httpx for direct API calls."""
        cookies = await self._context.cookies()
        return {c["name"]: c["value"] for c in cookies}


# ------------------------------------------------------------------
# Parsing helpers
# ------------------------------------------------------------------

def _parse_promotion_products(data: dict) -> "list[PromotionProduct]":
    products = []
    for item in data.get("PromotionOfferDetail", {}).get("ProductList", {}).get("List", []):
        p = item.get("PLP_Str", {})
        img = p.get("ImageURL", "")
        if img.startswith("//"):
            img = "https:" + img
        products.append(PromotionProduct(
            sku=p.get("SKU", ""),
            brand=p.get("Brand", ""),
            name=p.get("Name", ""),
            subtitle=p.get("Product_Subtitle", ""),
            slug=p.get("Slug", ""),
            image_url=img,
            price_original=_safe_float(item.get("Price_Original", "0")),
            price_new=_safe_float(p.get("NewPrice", "0")),
            label=item.get("DisplayInfo_Label", ""),
            is_available=p.get("IsAvailable", False),
            max_order_limit=p.get("MaxOrderLimit", 0),
        ))
    return products


def _parse_promotion_result(data: dict) -> "PromotionResult":
    period = data.get("PromotionPeriod", {})
    promotions: list[Promotion] = []

    for section in data.get("PromotionOfferList", {}).get("List", []):
        cat = section.get("Category", {})
        cat_id = cat.get("CategoryId", "")
        cat_label = cat.get("CategoryLabel", "")

        for offer in cat.get("Offers", {}).get("List", []):
            img = offer.get("ImageURL", "")
            if img.startswith("//"):
                img = "https:" + img

            promotions.append(Promotion(
                category_id=cat_id,
                category_label=cat_label,
                slug=offer.get("Slug", ""),
                brand=offer.get("Brand", ""),
                name=offer.get("Name", ""),
                subtitle=offer.get("Example", ""),
                variant=offer.get("Variant", ""),
                label=offer.get("DisplayInfo_Label", ""),
                price_new=_safe_float(offer.get("NewPrice", "0")),
                price_was=_safe_float(offer.get("PriceOriginal_Lowest", "0")),
                start_date=offer.get("StartDate", ""),
                end_date=offer.get("EndDate", ""),
                sku=offer.get("Product_SKU", ""),
                image_url=img,
                is_free_delivery=offer.get("IsFreeDeliveryOffer", False),
                is_single_product=offer.get("IsSingleProduct", False),
            ))

    return PromotionResult(
        period_from=period.get("FromDate", ""),
        period_to=period.get("ToDate", ""),
        is_next_week_published=data.get("IsNextWeekPublished", False),
        promotions=promotions,
    )


def _safe_float(value) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return 0.0


def _parse_price(text: str) -> float:
    """Extract a float price from strings like '€ 1,99', '1.99', '2,50'."""
    cleaned = re.sub(r"[^\d,.]", "", text).replace(",", ".")
    # Remove leading/trailing dots
    cleaned = cleaned.strip(".")
    if not cleaned:
        return 0.0
    try:
        return float(cleaned)
    except ValueError:
        return 0.0


def _parse_int(text: str) -> int:
    m = re.search(r"\d+", text)
    return int(m.group()) if m else 0
