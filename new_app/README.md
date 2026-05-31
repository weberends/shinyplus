# ShinyPLUS — Python rewrite

Direct API client for PLUS.nl, built on Playwright. Replaces browser-based R/Chromote automation with direct `fetch()` calls from inside a logged-in browser page — ~400ms per operation instead of 20–30s.

See [`ARCHITECTURE.md`](ARCHITECTURE.md) for how the PLUS.nl OutSystems API was reverse-engineered.

---

## Setup

```bash
pip install -r requirements.txt
playwright install chromium
```

Credentials file (`~/plus_credentials.yaml`):

```yaml
email: you@example.com
password: yourpassword
```

---

## Cart operations

```python
import asyncio
from plus.client import PlusClient

async def main():
    async with PlusClient(headless=True) as client:
        await client.login("you@example.com", "yourpassword")
        await client.get_session_state()   # captures CheckoutId, store info, apiVersions

        # Add 2 units of a product by SKU
        checkout = await client.add_to_cart_api("957806", quantity=2)
        print(f"Cart total: €{checkout['Receipt']['Price']}")

        # Remove 1 unit
        checkout = await client.remove_from_cart_api("957806", quantity=1)
        print(f"Cart total after remove: €{checkout['Receipt']['Price']}")

asyncio.run(main())
```

Output:

```
[+] Logged in as: MS
[+] Session state captured: checkout_id=801237f3… version=236 cart_module_version=aYUiHBQTI6…
[API] ActionCheckoutItem_Add → 200 in 412ms
Cart total: €12.45
[API] ActionCheckoutItem_Remove → 200 in 398ms
Cart total after remove: €8.23
```

First run performs a one-time browser click to capture action-specific `apiVersion` hashes; all subsequent runs load from `~/.config/shinyplus/api_versions.json` and skip the browser primes entirely.

---

## Current promotions

```python
import asyncio
from plus.client import PlusClient

async def main():
    async with PlusClient(headless=True) as client:
        await client.login("you@example.com", "yourpassword")
        await client.get_session_state()

        result = await client.get_promotions_api()

        print(f"Week {result.period_from} t/m {result.period_to}")
        print(f"{len(result.promotions)} aanbiedingen\n")

        for promo in result.promotions[:6]:
            print(f"  [{promo.category_label}] {promo.brand}  {promo.label}")

asyncio.run(main())
```

Output:

```
Week 2026-05-27 t/m 2026-06-02
148 aanbiedingen

  [Gratis bezorging] Danio, Danone, HiPRO  GRATIS BEZORGING BIJ 10.00 EURO
  [Aardappelen, groente, fruit] PLUS Bananen Fairtrade  0.99 PER KILO
  [Aardappelen, groente, fruit] PLUS Kersen
  [Verse kant-en-klaarmaaltijden] PLUS Verse pizza's  2 VOOR 6.99
  [Vlees, kip, vis, vega] Boerentrots van PLUS  1+1 GRATIS
  [Kaas, vleeswaren, tapas] Kaas stukken  1+1 GRATIS
```

Pass `next_week=True` for next week's offers (when published).

---

## Products behind a group promotion

Group promotions (`is_single_product=False`) cover a range of qualifying products. Fetch them on demand by slug:

```python
import asyncio
from plus.client import PlusClient

async def main():
    async with PlusClient(headless=True) as client:
        await client.login("you@example.com", "yourpassword")
        await client.get_session_state()

        result = await client.get_promotions_api()

        # Pick any group promotion
        group_deals = [p for p in result.promotions if not p.is_single_product and not p.is_free_delivery]
        promo = group_deals[0]
        print(f"{promo.brand}  {promo.label}  (slug: {promo.slug})\n")

        # Fetch the individual products qualifying for this deal
        products = await client.get_promotion_products_api(promo.slug)
        for p in products:
            avail = "✓" if p.is_available else "✗"
            print(f"  {avail} SKU {p.sku}  {p.name} ({p.subtitle})  €{p.price_original}  [{p.label}]")

asyncio.run(main())
```

Output:

```
Jan  1+1 GRATIS  (slug: 4431-96)

  ✗ SKU 432361  Rond & dun pizzadeeg duoverpakking (Per 520 g)  €2.99  [1+1 GRATIS]
  ✗ SKU 432363  Pizzadeeg zuurdesem met tomatensaus (Per 600 g)  €2.99  [1+1 GRATIS]
  ✗ SKU 523505  Napoli pizzadeeg 300g (Per 300 g)  €2.49  [1+1 GRATIS]
```

`is_available` reflects stock at store #868 (PLUS Wolters, Burgum) — the `StoreNumber` sent in the request.

---

## Test scripts

| Script | What it tests |
|---|---|
| `python test_pipeline.py ~/plus_credentials.yaml` | Login + 10-product basket via browser then direct API |
| `python test_cart_ops.py ~/plus_credentials.yaml` | Add + remove round-trip |
| `python test_promotions.py ~/plus_credentials.yaml` | Promotions list, current and `--next-week` |
