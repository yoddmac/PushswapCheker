NAME	= pushswap_checker

CC	= ghc

RM	= rm -f

SRCS	= pushswap_checker.hs

all: $(NAME)

$(NAME):
	$(CC) $(NAME)

clean:
	$(RM) $(NAME)

fclean: clean
	$(RM) pushswap_checker.hi
	$(RM) pushswap_checker.o

re: fclean all

.PHONY: all clean fclean re
