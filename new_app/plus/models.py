from __future__ import annotations
from dataclasses import dataclass, field
from pydantic import BaseModel, computed_field


class CartItem(BaseModel):
    product: str
    unit: str
    price: float
    quantity: int

    @computed_field
    @property
    def price_total(self) -> float:
        return round(self.price * self.quantity, 2)


class Cart(BaseModel):
    items: list[CartItem]
    final_total: float

    @property
    def total_items(self) -> int:
        return sum(i.quantity for i in self.items)

    def __str__(self) -> str:
        lines = [
            f"PLUS Winkelwagen — {len(self.items)} producten, {self.total_items} stuks",
            "-" * 62,
        ]
        for item in self.items:
            lines.append(
                f"  {item.quantity:>2}x  {item.product:<38}  €{item.price_total:>6.2f}"
            )
        lines.append("-" * 62)
        original = sum(i.price_total for i in self.items)
        savings = round(original - self.final_total, 2)
        lines.append(f"  Subtotaal:  €{original:>6.2f}")
        if savings > 0:
            lines.append(f"  Korting:   -€{savings:>6.2f}")
        lines.append(f"  Totaal:     €{self.final_total:>6.2f}")
        return "\n".join(lines)


# ---------------------------------------------------------------------------
# Promotions
# ---------------------------------------------------------------------------

@dataclass
class PromotionProduct:
    sku: str
    brand: str
    name: str
    subtitle: str       # "Per 520 g"
    slug: str
    image_url: str
    price_original: float   # pre-discount price (from outer Price_Original)
    price_new: float        # discounted price (PLP_Str.NewPrice, often 0 for group deals)
    label: str              # per-product deal label, e.g. "1+1 GRATIS"
    is_available: bool
    max_order_limit: int

    @property
    def url(self) -> str:
        return f"https://www.plus.nl/product/{self.slug}" if self.slug else ""


@dataclass
class Promotion:
    category_id: str
    category_label: str
    slug: str
    brand: str
    name: str           # product name — populated for single-product deals
    subtitle: str       # "Bijv. …" example sentence
    variant: str        # exception text, e.g. "M.U.V. losse ijsjes"
    label: str          # "1+1 GRATIS", "0.99 PER KILO", etc.
    price_new: float
    price_was: float    # PriceOriginal_Lowest from the API
    start_date: str
    end_date: str
    sku: str            # populated only when is_single_product is True
    image_url: str
    is_free_delivery: bool
    is_single_product: bool

    @property
    def url(self) -> str:
        return f"https://www.plus.nl/aanbiedingen/{self.slug}" if self.slug else ""


@dataclass
class PromotionResult:
    period_from: str
    period_to: str
    is_next_week_published: bool
    promotions: list[Promotion] = field(default_factory=list)

    def __str__(self) -> str:
        lines = [
            f"PLUS Aanbiedingen {self.period_from} t/m {self.period_to}",
            f"({len(self.promotions)} aanbiedingen, volgende week: {'gepubliceerd' if self.is_next_week_published else 'nog niet'})",
            "-" * 62,
        ]
        current_cat = ""
        for p in self.promotions:
            if p.category_label != current_cat:
                current_cat = p.category_label
                lines.append(f"\n  {current_cat}")
            label = f"[{p.label}]" if p.label else ""
            name = p.name or p.brand
            lines.append(f"    {name:<40} {label}")
        return "\n".join(lines)
