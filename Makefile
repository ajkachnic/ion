build: *.go
	go build .

test: build ./test/*
	node test/runner.mjs ./ion ./test/**/*.ion ./test/*.ion