WARNINGS  := -Wall -Wextra
OPT_LEVEL := -Os
STD       := -std=c++20 -stdlib=libc++
CXX       := clang++ -c -static
INCLUDES  := -Iinc

gc:
	$(CXX) $(INCLUDES) $(OPT_LEVEL) $(STD) $(WARNINGS) src/gc.cpp

test:
	$(CXX) $(INCLUDES) $(WARNINGS) src/test_main.cpp -fno-use-cxa-atexit -fno-exceptions -fno-rtti
