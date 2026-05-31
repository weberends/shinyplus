"""
Explore DataActionGetProductListAndCategoryInfo.

Navigates to https://www.plus.nl/aanbiedingen, intercepts all screenservices
requests/responses, and saves them to promotions_capture.json.

The goal: discover the exact URL, versionInfo, inputParameters, and response
shape for the promotions endpoint so it can be called directly as a clean API.

Usage:
    python explore_promotions.py path/to/credentials.yaml [--headful]
"""
from __future__ import annotations

import asyncio
import json
import sys
from pathlib import Path

import yaml

sys.path.insert(0, str(Path(__file__).parent))
from plus.client import PlusClient

_PROMO_URL = "https://www.plus.nl/aanbiedingen"
_OUT_FILE = Path(__file__).parent / "promotions_capture.json"


async def run(creds_path: Path, headless: bool) -> None:
    creds = yaml.safe_load(creds_path.read_text())
    email = creds["email"]
    password = creds["password"]
    if isinstance(email, list):
        email = email[0]
    if isinstance(password, list):
        password = password[0]

    captures: list[dict] = []
    pending: dict[str, dict] = {}  # url → entry (request body captured before response)
    response_futures: list[asyncio.Future] = []

    async with PlusClient(headless=headless) as client:
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
            endpoint = "/".join(response.url.split("/")[-2:])

            async def process() -> None:
                try:
                    entry["status"] = response.status
                    entry["response"] = await response.json()
                except Exception as exc:
                    entry["response_error"] = str(exc)
                captures.append(entry)
                print(f"  [←] {endpoint} → {entry.get('status', '?')}")

            fut = asyncio.ensure_future(process())
            response_futures.append(fut)

        page.on("request", on_request)
        page.on("response", on_response)

        print(f"[*] Inloggen als {email}…")
        ok = await client.login(email, password)
        if not ok:
            print("[-] Login mislukt.")
            sys.exit(1)

        print(f"\n[*] Navigeren naar {_PROMO_URL}…")
        print("    (intercepting all screenservices calls)\n")
        await page.goto(_PROMO_URL)
        await page.wait_for_load_state("networkidle", timeout=25_000)
        await asyncio.sleep(2)  # allow deferred/lazy requests to fire

        # Wait for all async response processors to finish
        if response_futures:
            await asyncio.gather(*response_futures, return_exceptions=True)

        # Identify the promotions endpoint(s)
        promo = [c for c in captures
                 if "GetProductList" in c["url"] or "CategoryInfo" in c["url"]]

        print(f"\n{'='*65}")
        if not promo:
            print("[!] DataActionGetProductListAndCategoryInfo NIET onderschept.")
            print("    Alle screenservices-endpoints op deze pagina:")
            for c in captures:
                ep = "/".join(c["url"].split("/")[-2:])
                print(f"      {ep}")
        else:
            print(f"[+] {len(promo)} promotions-endpoint(s) onderschept:\n")
            for entry in promo:
                url = entry["url"]
                req = entry.get("request", {})
                resp = entry.get("response", {})
                data = resp.get("data", {})

                print(f"  URL       : {url}")
                print(f"  Module    : {url.split('/')[-2]}")
                print(f"  Endpoint  : {url.split('/')[-1]}")
                print()

                vi = req.get("versionInfo", {})
                print(f"  versionInfo:")
                print(f"    moduleVersion : {vi.get('moduleVersion', '—')}")
                print(f"    apiVersion    : {vi.get('apiVersion', '—')}")
                print()

                params = req.get("inputParameters", {})
                print(f"  inputParameters:")
                print(json.dumps(params, indent=4, ensure_ascii=False))
                print()

                print(f"  Response data keys: {list(data.keys())}")
                _summarise_data(data, indent=4)
                print()

        # Save full capture regardless
        _OUT_FILE.write_text(json.dumps(captures, indent=2, ensure_ascii=False))
        print(f"\n[→] {len(captures)} captures opgeslagen in {_OUT_FILE}")
        print("    Analyseer met:  python -c \"import json; d=json.load(open('promotions_capture.json')); print(d[0]['url'])\"")


def _summarise_data(data: dict, indent: int = 0) -> None:
    """Recursively print structure of the response data without printing all items."""
    prefix = " " * indent
    for key, val in data.items():
        if isinstance(val, list):
            print(f"{prefix}{key}: list[{len(val)}]", end="")
            if val and isinstance(val[0], dict):
                print(f"  keys={list(val[0].keys())}")
            else:
                print()
        elif isinstance(val, dict):
            print(f"{prefix}{key}: dict  keys={list(val.keys())}")
            if len(val) <= 6:
                _summarise_data(val, indent + 2)
        else:
            # Scalar — print it directly (useful for periods, counts, etc.)
            print(f"{prefix}{key}: {val!r}")


def main() -> None:
    args = sys.argv[1:]
    headless = "--headful" not in args
    args = [a for a in args if not a.startswith("--")]

    if not args:
        print(__doc__)
        sys.exit(1)

    creds_path = Path(args[0])
    if not creds_path.exists():
        print(f"[FOUT] Credentials bestand niet gevonden: {creds_path}")
        sys.exit(1)

    asyncio.run(run(creds_path, headless=headless))


if __name__ == "__main__":
    main()
