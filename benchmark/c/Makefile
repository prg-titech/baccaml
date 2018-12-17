APP = fib
CC = gcc
CFLAGS = 

$(APP): $(APP).c time.c
	$(CC) -o $(APP) $(CFLAGS) $^

$(APP)-m32: $(APP).c time.c
	$(CC) -m32 -o $(APP) $(CFLAGS) $^

clean:
	$(RM) $(APP)
