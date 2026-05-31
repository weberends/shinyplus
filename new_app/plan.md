# ShinyPLUS → Python Migration Plan

> **Full architecture plan is below. Current task: MVP only — validate PLUS.nl login + add-to-basket pipeline before building the full app.**

---

## MVP Scope: Validate PLUS.nl Pipeline

Build the minimum needed to confirm the Python → PLUS.nl pipeline is viable:

1. **Login** — Playwright-based login to PLUS.nl (same OAuth2 flow as R/Chromote, but Python-native async)
2. **Add one product to basket** — browser automation via Playwright, with network interception to discover direct REST API endpoints for future use
3. **Confirm the cart** — parse cart HTML and verify the item appears

Run `python test_pipeline.py path/to/credentials.yaml [sku]` to exercise the full pipeline end-to-end.

### MVP file structure
```
new_app/
├── plus/
│   ├── __init__.py
│   ├── client.py       # Playwright async: login, add_to_cart, get_cart + API discovery
│   └── models.py       # Pydantic: CartItem, Cart
├── test_pipeline.py    # Run: login → read cart → add SKU → confirm cart
├── plan.md             # This file
└── requirements.txt    # playwright, pydantic, pyyaml, httpx
```

---

## Recommended Stack (full app)

### Framework: **NiceGUI**

NiceGUI is the best fit. Reasons:

| Requirement | Why NiceGUI wins |
|---|---|
| Real-time async updates (cart-fill progress) | Runs on asyncio + WebSockets; `async def` throughout |
| Drag-and-drop (fixed products, ingredients) | Built-in `ui.sortable` with `on_reorder` callback |
| Modals (ingredient selection, confirmations) | `ui.dialog` / `ui.notification` |
| Image-rich tables (cart contents with product images) | `ui.aggrid` + HTML cell renderers |
| Complex multi-column layout | Bootstrap-compatible `ui.row` / `ui.column` + `ui.tabs` |
| Custom PLUS branding (CSS variables) | `ui.add_css()` and `ui.html()` escape hatches |
| FastAPI backend (for product search API) | NiceGUI IS FastAPI; `@app.get()` works out of the box |
| Personal tool simplicity | Single-file entry point, no build step |

**Why not the alternatives:**
- **Streamlit** — re-runs entire script on every event; the 30+ fine-grained reactive handlers are impossible.
- **Reflex** — Python→React compile step adds friction; real-time async progress is awkward at this maturity level.
- **FastAPI + HTMX** — requires hand-writing HTML fragments for every modal and custom dropdown.
- **Panel/Solara** — weak component ecosystems for drag-drop + image dropdowns.

### PLUS Integration: **Direct REST API** (with Playwright fallback)

PLUS.nl's web frontend is a React SPA calling an internal REST API. The OAuth2 `client_id=web_ecop_eprod` is already visible in the current R code. After confirming the MVP pipeline works, capture network requests during a real session to discover direct API endpoints for login, cart, promotions, and product search.

**Expected API surface:**
- `POST /auth/token` — exchange email+password for Bearer token
- `GET /cart` — read cart contents
- `POST /cart/items` — add item by SKU + quantity
- `GET /promotions` — current sale items
- `GET /products/search?q=...` — product search

This replaces the 10–30 second browser-click loop with sub-2-second direct calls.

### Data Storage: **SQLite + SQLAlchemy Core + aiosqlite**

| RDS file | SQLite table | Notes |
|---|---|---|
| `product_list.rds` | `products` | name, unit, url, img; index on url |
| `dishes-{email}.rds` | `dishes` | + `user_id` FK |
| `dish_ingredients-{email}.rds` | `dish_ingredients` | product_url, qty, label, optional flag |
| `weekmenu-{email}.rds` | `weekmenu` | 12 slots (7 dinner + 5 lunch) |
| `fixed_products-{email}.rds` | `fixed_products` | + integer `sort_order` for drag-drop |
| `basket-{email}.rds` | `basket` | product_url, quantity, label |

---

## Full Project Structure

```
new_app/
├── main.py                    # NiceGUI entry point
├── config.py                  # Credentials YAML loading
├── requirements.txt
├── db/
│   ├── schema.py
│   ├── migrations.py          # One-time RDS → SQLite import
│   └── queries.py
├── plus/
│   ├── client.py              # Playwright + httpx async client
│   └── models.py
├── ui/
│   ├── theme.py               # PLUS brand CSS variables
│   ├── product_select.py      # Image + subtext autocomplete widget
│   └── tabs/
│       ├── boodschappen.py
│       ├── mandje.py
│       ├── gerechten.py
│       ├── artikelen.py
│       └── account.py
├── services/
│   ├── basket.py
│   ├── weekmenu.py            # ICS generation
│   └── email.py               # Gmail SMTP
└── data/
    └── shinyplus.db
```

---

## Key Migration Challenges

1. **Image dropdowns** — NiceGUI `ui.select` renders plain text. Use Quasar `QSelect` with a scoped `option` slot, or a `/api/products/search` FastAPI endpoint + custom input widget (~40 lines JS).
2. **Optional/flexible ingredient modal** — `ui.dialog` with a loop of product-select widgets. No `sendCustomMessage` round-trip needed.
3. **Add-to-cart progress** — `async def` with `progress_bar.set_value(i/n)` inside the loop; NiceGUI's WebSocket pushes updates natively.
4. **Data migration** — `Rscript -e "write.csv(readRDS('dishes.rds'), '/tmp/dishes.csv')"` then INSERT into SQLite.
5. **iCal** — `icalendar` Python package is a direct replacement.
6. **Email** — `smtplib` + `email.mime` replaces `blastula`.

## Implementation Order (full app)

1. NiceGUI skeleton + PLUS CSS theme
2. `db/schema.py` + `db/migrations.py`
3. `plus/client.py` — direct API (post-MVP)
4. `ui/product_select.py` — image autocomplete (unblocks all tabs)
5. `ui/tabs/gerechten.py` — pure CRUD, no PLUS dependency
6. `ui/tabs/boodschappen.py` — weekmenu + promotions + fixed products
7. `ui/tabs/mandje.py` — basket + async cart-fill
8. `services/email.py` + `services/weekmenu.py`
9. `ui/tabs/artikelen.py`

---

## Critical Source Files for Reference

| File | What to carry forward |
|---|---|
| `R/plus_remote_functions.R` | OAuth2 `client_id`, cart HTML selectors, add-to-cart logic |
| `R/utils.R` | CSS selectors for promotions HTML scrape |
| `R/shinyplus.R` | UI layout, CSS variables, reactive state patterns, all modals |
| `R/send_email.R` | HTML email templates |
| `R/zzz.R` | Data directory resolution logic |
