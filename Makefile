CXX := g++
CXXFLAGS := -O3 -std=c++11

ifeq ($(OS),Windows_NT)
EXE := .exe
else
EXE :=
endif

TARGET := dolmatch$(EXE)

.PHONY: all

all: $(TARGET)

clean:
	rm -f dolmatch dolmatch.exe

$(TARGET): dolmatch.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^
