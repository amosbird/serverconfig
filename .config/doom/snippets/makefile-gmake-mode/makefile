# -*- mode: snippet -*-
# name: makefile
# key: makefile
# expand-env: ((yas-indent-line nil))
# --
MKDIR_P ?= mkdir -p

BUILD_DIR ?= ./build
OBJ_DIR := $(BUILD_DIR)/objs
BIN_DIR := $(BUILD_DIR)/bin

TARGET_EXEC ?= $(BIN_DIR)/funny

SRC_DIR ?= ./src
TEST_DIR ?= ./test

CPPSRCS := $(shell find $(SRC_DIR) -type f -name *.cpp)
CSRCS := $(shell find $(SRC_DIR) -type f -name *.c)
SSRCS := $(shell find $(SRC_DIR) -type f -name *.s)
CPPFILES := $(CPPSRCS:$(SRC_DIR)/%.cpp=$(BIN_DIR)/%)
CFILES := $(CSRCS:$(SRC_DIR)/%.c=$(BIN_DIR)/%)
SFILES := $(SSRCS:$(SRC_DIR)/%.s=$(BIN_DIR)/%)

SRCS := $(CPPSRCS) $(CSRCS) $(SSRCS)
OBJS := $(SRCS:%=$(OBJ_DIR)/%.o)
DEPS := $(OBJS:.o=.d)

INC_DIRS := $(shell find $(SRC_DIR) -type d)
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

CFLAGS ?= -std=gnu11 -fopenmp
CXXFLAGS ?= -std=gnu17
CPPFLAGS ?= $(INC_FLAGS) -MMD -MP
LD = gcc
LDFLAGS ?= -lomp

print-%:
    @echo '$*=$($*)'

test: $(CPPFILES) $(CFILES) $(SFILES)

$(TARGET_EXEC): $(OBJS)
	@$(MKDIR_P) $(dir $@)
	$(LD) $(LDFLAGS) $< -o $@

$(CPPFILES): $(BIN_DIR)/%: $(OBJ_DIR)/%.cpp.o
	@$(MKDIR_P) $(dir $@)
	$(LD) $(LDFLAGS) $< -o $@

$(CFILES): $(BIN_DIR)/%: $(OBJ_DIR)/%.c.o
	@$(MKDIR_P) $(dir $@)
	$(LD) $(LDFLAGS) $< -o $@

$(SFILES): $(BIN_DIR)/%: $(OBJ_DIR)/%.s.o
	@$(MKDIR_P) $(dir $@)
	$(LD) $(LDFLAGS) $< -o $@

# assembly
$(OBJ_DIR)/%.s.o: $(SRC_DIR)/%.s
	@$(MKDIR_P) $(dir $@)
	$(AS) $(ASFLAGS) -c $< -o $@

# c source
$(OBJ_DIR)/%.c.o: $(SRC_DIR)/%.c
	@$(MKDIR_P) $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

# c++ source
$(OBJ_DIR)/%.cpp.o: $(SRC_DIR)/%.cpp
	@$(MKDIR_P) $(dir $@)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

SOURCE := $(shell find $(SRC_DIR) -type f -name *.cpp -or -name *.c -or -name *.s | sort | xargs shasum)

# used to detect source file deletion
$(BUILD_DIR)/.source:
	@$(MKDIR_P) $(dir $@)
	@echo $(SOURCE) | cmp -s - $@ || echo $(SOURCE) > $@

clean:
	$(RM) -r $(BUILD_DIR)

-include $(DEPS)

.PHONY: clean $(BUILD_DIR)/.source test