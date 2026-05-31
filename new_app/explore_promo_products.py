"""
Explore which API endpoint fires when navigating to a promotion detail page.

Navigates to https://www.plus.nl/aanbiedingen/{slug} and intercepts all
screenservices calls to find the endpoint that returns individual products
for a non-single-product promotion.

Usage:
    python explore_promo_products.py path/to/credentials.yaml [slug]

slug defaults to '4431-96' (Jan 1+1 GRATIS — a group promotion).
"""
from __future__ import annotations

import asyncio
import json
import sys
from pathlib import Path

import yaml

sys.path.insert(0, str(Path(__file__).parent))
from plus.client import PlusClient

_OUT_FILE = Path(__file__).parent / "promo_products_capture.json"


async def run(creds_path: Path, slug: str) -> None:
    creds = yaml.safe_load(creds_path.read_text())
    email = creds["email"]
    password = creds["password"]
    if isinstance(email, list): email = email[0]
    if isinstance(password, list): password = password[0]

    captures: list[dict] = []
    pending: dict[str, dict] = {}
    response_futures: list[asyncio.Future] = []

    async with PlusClient(headless=True) as client:
        page = client._page

        def on_request(request) -> None:
            if "screenservices" not in request.url or request.method != "POST":
                return
            endpoint = "/".join(request.url.split("/")[-2:])
            body = request.post_data_json or {}
            pending[request.url] = {"url": request.url, "request": body}
            print(f"  [→] {endpoint}")

        def on_response(response) -> None:
            if "screenservices" not in response.url:
                return
            entry = pending.pop(response.url, {"url": response.url, "request": {}})

            async def process() -> None:
                entry["status"] = response.status
                try:
                    entry["response"] = await response.json()
                except Exception as exc:
                    entry["response_error"] = str(exc)
                captures.append(entry)
                print(f"  [←] {'/'.join(response.url.split('/')[-2:])} → {entry['status']}")

            response_futures.append(asyncio.ensure_future(process()))

        page.on("request", on_request)
        page.on("response", on_response)

        print(f"[1/3] Inloggen als {email}…")
        ok = await client.login(email, password)
        if not ok:
            print("[-] Login mislukt")
            sys.exit(1)

        promo_url = f"https://www.plus.nl/aanbiedingen/{slug}"
        print(f"\n[2/3] Navigeren naar {promo_url}…\n")
        await page.goto(promo_url)
        await page.wait_for_load_state("networkidle", timeout=25_000)
        await asyncio.sleep(2)

        if response_futures:
            await asyncio.gather(*response_futures, return_exceptions=True)

        print(f"\n[3/3] Analyse van {len(captures)} onderschepte calls:\n")

        # Separate login-phase calls from promo-page calls by looking for product-related endpoints
        promo_page_endpoints = [
            "GetProductList", "CategoryInfo", "PromotionDetail", "Promotion",
            "Product", "Search", "PLP", "Offer"
        ]
        interesting = [
            c for c in captures
            if any(kw.lower() in c["url"].lower() for kw in promo_page_endpoints)
            and "PromotionList_Optimization" not in c["url"]
            and "PromotionPeriod" not in c["url"]
        ]

        if not interesting:
            print("[!] Geen product-gerelateerde endpoint gevonden.")
            print("    Alle onderschepte screenservices calls:")
            for c in captures:
                ep = "/".join(c["url"].split("/")[-2:])
                data_keys = list(c.get("response", {}).get("data", {}).keys())
                print(f"      {ep}  →  data keys: {data_keys}")
        else:
            for entry in interesting:
                url = entry["url"]
                data = entry.get("response", {}).get("data", {})
                print(f"URL: {url}")
                print(f"Module/Endpoint: {'/'.join(url.split('/')[-2:])}")
                req = entry.get("request", {})
                vi = req.get("versionInfo", {})
                print(f"versionInfo: moduleVersion={vi.get('moduleVersion','—')[:20]}  apiVersion={vi.get('apiVersion','—')[:20]}")
                params = req.get("inputParameters") or req.get("screenData", {}).get("variables", {})
                print(f"Parameters (top-level keys): {list(params.keys())[:12]}")
                print(f"Response data keys: {list(data.keys())}")
                _show_list_lengths(data)
                print()

        _OUT_FILE.write_text(json.dumps(captures, indent=2, ensure_ascii=False))
        print(f"\n[→] Alle {len(captures)} captures opgeslagen in {_OUT_FILE}")


def _show_list_lengths(data: dict, prefix: str = "  ") -> None:
    for k, v in data.items():
        if isinstance(v, list):
            sample_keys = list(v[0].keys()) if v and isinstance(v[0], dict) else []
            print(f"{prefix}{k}: list[{len(v)}]  {sample_keys[:8]}")
        elif isinstance(v, dict):
            inner_lists = {kk: len(vv) for kk, vv in v.items() if isinstance(vv, list)}
            if inner_lists:
                print(f"{prefix}{k}: dict  lists={inner_lists}")
            else:
                print(f"{prefix}{k}: {v!r}"[:120])


def main() -> None:
    args = sys.argv[1:]
    args = [a for a in args if not a.startswith("--")]
    if not args:
        print(__doc__)
        sys.exit(1)
    creds_path = Path(args[0])
    slug = args[1] if len(args) > 1 else "4431-96"
    if not creds_path.exists():
        print(f"[FOUT] {creds_path} niet gevonden")
        sys.exit(1)
    asyncio.run(run(creds_path, slug))


if __name__ == "__main__":
    main()
