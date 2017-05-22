
SRC = src
S = scala
SC = scalac
TARGET = out

default:
	@echo "Compiling..."
	@mkdir out
	@$(SC) -d $(TARGET) $(SRC)/*.scala

clean:
	@rm -r $(TARGET)
