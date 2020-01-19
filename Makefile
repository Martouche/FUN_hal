all:
	stack build --copy-bins --local-bin-path .

clean:
	stack clean
	rm -rf .stack-work
	rm -rf evalexpr.cabal

fclean: clean
	rm -f hal

re: fclean all
