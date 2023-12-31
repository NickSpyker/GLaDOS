##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## Makefile
##

NAME = glados

TEST_PATH := $(shell stack path --dist-dir)
COVERAGE_PATH = ./test/coverage
TEST_NAME = ./unit_tests

VERSION := $(shell cat package.yaml | grep "version:" | cut -d : -f 2 | xargs)

.PHONY: all release tests_run coverage bonus clean fclean re

all:
	@stack build --copy-bins --local-bin-path ./
	@mv $(NAME)-exe $(NAME)

release: all
	@mv $(NAME) $(NAME)-v$(VERSION)

tests_run:
	@stack build --test --coverage
	@cp "$(TEST_PATH)/build/$(NAME)-test/$(NAME)-test" ./$(TEST_NAME)
	@./$(TEST_NAME)
	@$(RM) -r $(COVERAGE_PATH)
	@mkdir $(COVERAGE_PATH)
	@mv $(TEST_NAME).tix $(COVERAGE_PATH)/

coverage: fclean tests_run
	@stack hpc report --all --destdir $(COVERAGE_PATH)
	@firefox ./test/coverage/hpc_index.html

bonus:
	@make -C ./bonus

clean:
	@stack clean
	@make clean -C ./bonus

fclean: clean
	@$(RM) -r $(COVERAGE_PATH) ./.stack-work ./out
	@$(RM) $(NAME) $(TEST_NAME) $(TEST_NAME).tix ./stack.yaml.lock ./glados.cabal
	@$(RM) $(NAME)-v$(VERSION)
	@make fclean -C ./bonus

re: fclean all
