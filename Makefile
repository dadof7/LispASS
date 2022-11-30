BASIC_LISP = init.l basic.l
ALL_LISP = $(BASIC_LISP) my.l

%.js: %.l
	@echo "var __$*__ = (function() {/*" > $@
	@cat $^ js.tail >> $@

all.js: 

all.l: $(ALL_LISP)
	cat $^ | sed -e 's/\/\*/\/\\\*/g' -e 's/\*\//\*\\\//g' > $@

clean:
	rm -f all.l all.js

# $@ : ターゲットファイル名
# $% : ターゲットがアーカイブメンバだったときのターゲットメンバ名
# $< : 最初の依存するファイルの名前
# $? : ターゲットより新しいすべての依存するファイル名
# $^ : すべての依存するファイルの名前
# $+ : Makefileと同じ順番の依存するファイルの名前
# $* : サフィックスを除いたターゲットの名前
