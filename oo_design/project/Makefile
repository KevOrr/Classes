# http://hiltmon.com/blog/2013/07/03/a-simple-c-plus-plus-project-structure/

SRCDIR := src
SRCEXT := cpp
INCDIR := include
INCEXT := hpp
BUILDDIR := build
OBJEXT := o

CXX := g++
CXXFLAGS := -std=c++11 -Wall
INC := -I $(INCDIR)

SOURCES := $(shell find $(SRCDIR) -type f -name *.$(SRCEXT))
HEADERS := $(patsubst $(SRCDIR)/%, $(INCDIR)/%, $(SOURCES:.$(SRCEXT)=.$(INCEXT)))
OBJECTS := $(patsubst $(SRCDIR)/%, $(BUILDDIR)/%, $(SOURCES:.$(SRCEXT)=.$(OBJEXT)))
TARGET := bin/university


# Linking
$(TARGET): $(OBJECTS)
	@mkdir -p -- $(shell dirname -- $@)
	@echo " Linking..."
	$(CXX) -o $@ $^

# Compilation
$(BUILDDIR)/%.$(OBJEXT): $(SRCDIR)/%.$(SRCEXT) $(INCDIR)/%.$(INCEXT)
	@mkdir -p $(BUILDDIR)
	$(CXX) $(CXXFLAGS) $(INC) -c -o $@ $<

clean:
	@echo " Cleaning...";
	@rm -r $(BUILDDIR) $(TARGET) 2>/dev/null || true

