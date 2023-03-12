##
## EPITECH PROJECT, 2023
## B-FUN-400-COT-4-1-wolfram-nicaise.gbenou
## File description:
## Makefile
##

SRC	:=	$(shell stack path --local-install-root)

NAME	=	wolfram

all:
		stack build
		cp $(SRC)/bin/Wolfram-exe	$(NAME)

fclean:
	rm -f $(NAME)
	rm -f *.o
	rm -f *.~

re:	fclean all
