##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## Makefile
##

NAME = code_generator
RELEASE = ./target/release/$(NAME)

all:
	@cargo build --package $(NAME) --bin $(NAME) --release
	@cp $(RELEASE) ../../code_gen

load_dependencies:
	@cargo vendor

tests_run:
	@cargo test --package $(NAME) --bin $(NAME)

clean:
	@cargo clean

fclean: clean
	@$(RM) ../../code_gen

re: fclean all
