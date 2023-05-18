(local Stack {})

(tset Stack :__index Stack)

(fn Stack.new [self ...]
    (let [stack {:values [...]}]
        (setmetatable stack self)
        stack))

(fn Stack.push [self ...]
    (each [_ v (ipairs [...])]
        (table.insert (. self :values) v)))

(fn Stack.pop [self]
    (table.remove (. self :values)))

(fn Stack.empty? [self]
    (= 0 (length (. self :values))))

(fn Stack.peek [self]
    (let [vv (. self :values)]
        (. vv (length vv))))

(fn Stack.__len [self] (length (. self :values)))

(fn Stack.__call [self ...]
    (self:push ...)
    (ipairs (. self :values)))

(fn Stack.__concat [self other]
    (let [vv (. other :values)]
        (self (table.unpack vv))))

(fn Stack.__tostring [self]
    (let [vv (icollect [_ v (self)]
                (tostring v))
          str (table.concat vv ", ")]
         (.. "[" str "]")))

Stack