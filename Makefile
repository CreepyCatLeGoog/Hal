##
## EPITECH PROJECT, 2021
## Makefile
## File description:
## Rules
##

CC = stack build
NAME = hal
EXE = hal-exe

all: $(NAME)

$(NAME):
	$(CC) --copy-bins --local-bin-path .
	mv $(EXE) $(NAME)

clean:
	stack clean

fclean: clean
		rm $(NAME)

purge:
	stack purge
	rm $(NAME)

re: fclean all