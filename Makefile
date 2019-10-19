PROJ=dosti
MODULE=mgenv

dev:
	stack test --fast --haddock-deps --file-watch

setup_hoogle:
	stack hoogle -- generate --local

hoogle:
	stack hoogle -- server --local --port=8080

build:
	stack build
