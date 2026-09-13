module HQ.Stream where

-- data JSONStep = FieldStep Text | EachStep deriving (Eq, Show)

-- data JSONPlan = Root | FieldPlan Text JSONPlan | EachPlan JSONPlan
--   deriving (Eq, Show)

-- instance Semigroup JSONPlan where
--   Root <> b = b
--   FieldPlan name rest <> b = FieldPlan name (rest <> b)
--   EachPlan rest <> b = EachPlan (rest <> b)

-- instance Monoid JSONPlan where mempty = Root

-- type JSONStreamParser = JS.Parser JS.Value

-- compile :: Optic -> JSONStreamParser
-- compile (Optic optic) = jsonParser $ foldFix algebra optic
--   where
--     algebra :: OpticF JSONPlan -> JSONPlan
--     algebra (Field name) = FieldPlan name Root
--     algebra Each = EachPlan Root
--     algebra (Compose a b) = a <> b

--     jsonParser :: JSONPlan -> JS.Parser JS.Value
--     jsonParser Root = JS.value
--     jsonParser (FieldPlan name rest) = name .: jsonParser rest
--     jsonParser (EachPlan rest) = arrayOf $ jsonParser rest

-- runJSON :: Optic -> BL.ByteString -> [JS.Value]
-- runJSON optic = JS.parseLazyByteString $ compile optic
