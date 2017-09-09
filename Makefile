b=dist/build/invoice/invoice

invoice.html: $b template.html content.json
	cabal run invoice

$b: invoice.hs
	cabal build
