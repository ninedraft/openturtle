(local Stack (require :stack))

(fn append [array ...]
    (each [_ arg (ipairs [...])]
        (table.insert array arg)))

(fn flop [tape top token]
    (local i (length tape))
    (match top
        [token pos] (do
                        (local gate (. tape pos))
                        (append (. tape (length tape)) pos)
                        (append gate i))
        _ (error (string.format "Unexpected token %q at %s" token i))))

(local token-mt {
    :__tostring (fn [token] 
        (let [strs (icollect [_ arg (ipairs token)]
                        (tostring arg))]
            (string.format "[%s]" (table.concat strs " "))))
})

(fn tok! [...]
    (let [token [...]]
        (setmetatable token token-mt)
        token))

(fn str-of [lit]
    (let [ body (lit:sub 2)
           (spaced _) (body:gsub "_" " ")]
            spaced))

(fn parse-chunk [input]
    (let [ stack (Stack:new)
            tape [] ]
        (each [token (input:gmatch "%S+")]
        (let [prefix (token:sub 1 1)
             x (tonumber token)]
                (case [prefix token]
                (where [_ _] x) (append tape (tok! :number x))
                [_ "true"] (append tape (tok! :bool true))
                [_ "false"] (append tape (tok! :bool false))
                [_ "if"] (do
                    (append tape (tok! :if))
                    (stack:push (tok! :if (length tape))))
                [_ "then"] (flop tape (stack:pop) :if)
                [_ "begin"] (do
                    (append tape (tok! :begin))
                    (stack:push (tok! :begin (length tape))))
                [_ "until"] (do
                    (append tape (tok! :until))
                    (flop tape (stack:pop) :begin))
                [_ "do"] (do
                    (append tape (tok! :do))
                    (stack:push (tok! :do (length tape))))
                [_ "loop"] (do
                    (append tape (tok! :loop))
                    (flop tape (stack:pop) :do))
                ["." str] (append tape (tok! :string (str-of str)))
                [:: word-def] (do
                    (append tape (tok! :word-def word-def))
                    (stack:push (tok! :word-def (length tape))))
                [_ ";"] (do
                    (append tape (tok! :word-end))
                    (flop tape (stack:pop) :word-def))
                [_ word-call] (append tape (tok! :call word-call)))))
        (values tape stack)))

(fn parse [input]
    (let [(tape stack) (parse-chunk input)]
            (when (not (stack:empty?))
                (error (string.format "Unmatched control flow %s" (tostring stack))))
                tape))

(local binary-ops {
    :+ #(+ $1 $2)
    :- #(- $1 $2)
    :* #(* $1 $2)
    :/ #(/ $1 $2)
    :% #(% $1 $2)
    :< #(> $1 $2)
    :> #(< $1 $2)
    :<= #(>= $1 $2)
    :>= #(<= $1 $2)
    := #(= $1 $2)
    :not= #(not= $1 $2)
    :and #(and $1 $2)
    :or #(or $1 $2)
})

(fn index-of [array test i]
    (case (. array i)
        (where _ (> i (length array))) nil 
        (where elem (test elem)) i
        _ (index-of array test (+ i 1))))

(fn index-of-token [tape token i]
    (index-of tape #(= (. $1 1) token) i))

(fn run [tape dict]
    (var i 1)
    (let [ dict (or dict {})
           stack (Stack:new)
           calls (Stack:new) ]
        (while (<= i (length tape))
            (local token (. tape i))
            (case token
            [:bool b] (stack:push b)
            [:number x] (stack:push x)
            [:string str] (stack:push str)
            (where [:call op] (. binary-ops op)) (let [
                o (. binary-ops op)
                a (stack:pop)
                b (stack:pop)]
                (stack:push (o a b)))
            (where [:call word] (. dict word)) (let
                [ f (. dict word) ]
                (calls:push i)
                (set i (. dict word)))
            [:call :I] (stack:push (calls:peek))
            [:begin to] (let
                [b (stack:pop)]
                (if b
                    (set i to)))
            [:if to] (let 
                [b (stack:pop)]
                (if (not b)
                    (set i to)))
            [:call :print] (let
                [x (stack:pop)]
                (print x))
            [:call :dump-stack] (print stack)
            [:call :swap] (let
                [a (stack:pop)
                 b (stack:pop)]
                 (stack:push a b))
            [:until to] (let
                [b (stack:pop)]
                (if b
                    (set i to)))
            [:do end] (let
                [ counter (stack:pop)
                  limit (stack:pop) ]
                (if (<= counter limit)
                    (do (calls:push limit)
                        (calls:push counter))
                    (set i end)))
            [:loop to] (let
                [ counter (+ (calls:pop) 1)
                  limit (calls:pop) ]
                  (when (<= counter limit)
                        (set i to)
                        (calls:push limit)
                        (calls:push counter)))
            [:word-def word] (let
                 [ name (word:sub 2) ]
                 (tset dict name i)
                 (set i (index-of-token tape :word-end i)))
            [:word-end _] (set i (calls:pop)))
            (set i (+ i 1)))))

