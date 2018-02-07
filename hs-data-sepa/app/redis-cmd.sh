# categoryを全て消す(デリミタが重要。これによって入力のクォートなどが効果を無くす)
# これはBSD版の古いものでは使えない
redis-cli keys "category:*" | xargs -d'\n' redis-cli del
