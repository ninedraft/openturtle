
(local utf8pat utf8.charpattern)

(fn utf8iter [str]
    (string.gmatch str utf8pat))

(fn fields [str]
    (accumulate [tokens []
                 char (utf8iter str)]))