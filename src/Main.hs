----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
import           Miso.Reload
----------------------------------------------------------------------------
-- | Calculator model
data Model = Model
  { _display  :: MisoString  -- current display string
  , _acc      :: Double      -- accumulated left-hand value
  , _op       :: Maybe Op    -- pending operator
  , _fresh    :: Bool        -- True  => next digit clears display
  } deriving (Show, Eq)
----------------------------------------------------------------------------
data Op = Add | Sub | Mul | Div deriving (Show, Eq)
----------------------------------------------------------------------------
display :: Lens Model MisoString
display = lens _display $ \r f -> r { _display = f }
----------------------------------------------------------------------------
acc :: Lens Model Double
acc = lens _acc $ \r f -> r { _acc = f }
----------------------------------------------------------------------------
op :: Lens Model (Maybe Op)
op = lens _op $ \r f -> r { _op = f }
----------------------------------------------------------------------------
fresh :: Lens Model Bool
fresh = lens _fresh $ \r f -> r { _fresh = f }
----------------------------------------------------------------------------
data Action
  = Digit MisoString
  | Dot
  | Operator Op
  | Equals
  | Clear
  | PlusMinus
  | Percent
  deriving (Show, Eq)
----------------------------------------------------------------------------
main :: IO ()
#ifdef INTERACTIVE
main = live defaultEvents app
#else
main = startApp defaultEvents app
#endif
----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------
app :: App Model Action
app = component emptyModel updateModel viewModel
----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model "0" 0 Nothing True
----------------------------------------------------------------------------
-- | Evaluate pending operation
applyOp :: Op -> Double -> Double -> Double
applyOp Add a b = a + b
applyOp Sub a b = a - b
applyOp Mul a b = a * b
applyOp Div a b = if b == 0 then 0 else a / b
----------------------------------------------------------------------------
-- | Format a Double for display (drop trailing ".0")
showNum :: Double -> MisoString
showNum n
  | n == fromIntegral (round n :: Int) = ms (show (round n :: Int))
  | otherwise                           = ms (show n)
----------------------------------------------------------------------------
updateModel :: Action -> Effect parent Model Action
updateModel = \case

  Digit d -> do
    fr <- use fresh
    if fr
      then do
        display .= d
        fresh   .= False
      else do
        cur <- use display
        let next = if cur == "0" then d else cur <> d
        display .= next

  Dot -> do
    fr  <- use fresh
    cur <- use display
    let base = if fr then "0" else cur
    if '.' `elem` (fromMisoString base :: String)
      then pure ()
      else do
        display .= base <> "."
        fresh   .= False

  PlusMinus -> do
    cur <- use display
    let n = read (fromMisoString cur) :: Double
    display .= showNum (negate n)

  Percent -> do
    cur <- use display
    let n = read (fromMisoString cur) :: Double
    display .= showNum (n / 100)

  Operator o -> do
    cur <- use display
    let n = read (fromMisoString cur) :: Double
    acc     .= n
    op      .= Just o
    fresh   .= True

  Equals -> do
    cur  <- use display
    lhs  <- use acc
    mOp  <- use op
    let rhs    = read (fromMisoString cur) :: Double
        result = case mOp of
                   Just o  -> applyOp o lhs rhs
                   Nothing -> rhs
    display .= showNum result
    acc     .= result
    op      .= Nothing
    fresh   .= True

  Clear ->
    put emptyModel
----------------------------------------------------------------------------
-- | View
viewModel :: Model -> View Model Action
viewModel m =
  vfrag
    [ vfrag
        [ H.p_
          [ P.class_ "calc-title" ]
          [ H.a_ [P.href_ "https://github.com/haskell-miso/miso-calculator" ] [ text "🍜 miso calculator" ] ]
        ]
    , H.div_ [ P.class_ "calc" ]
        [ -- Display row
          vfrag
            [ H.div_ [ P.class_ "display" ]
                [ H.span_ [ P.class_ "display-text" ] [ text (m ^. display) ] ]
            ]
          -- Button grid
        , vfrag
            [ H.div_ [ P.class_ "grid" ] $
                -- Row 1
                [ btn "fn"  (text "AC")  Clear
                , btn "fn"  (text "+/-") PlusMinus
                , btn "fn"  (text "%")   Percent
                , btn "op"  (text "÷")   (Operator Div)
                -- Row 2
                , btn "num" (text "7")   (Digit "7")
                , btn "num" (text "8")   (Digit "8")
                , btn "num" (text "9")   (Digit "9")
                , btn "op"  (text "×")   (Operator Mul)
                -- Row 3
                , btn "num" (text "4")   (Digit "4")
                , btn "num" (text "5")   (Digit "5")
                , btn "num" (text "6")   (Digit "6")
                , btn "op"  (text "−")   (Operator Sub)
                -- Row 4
                , btn "num" (text "1")   (Digit "1")
                , btn "num" (text "2")   (Digit "2")
                , btn "num" (text "3")   (Digit "3")
                , btn "op"  (text "+")   (Operator Add)
                -- Row 5
                , H.button_
                    [ P.class_ "btn num zero", H.onClick (Digit "0") ]
                    [ text "0" ]
                , btn "num" (text ".")   Dot
                , btn "eq"  (text "=")   Equals
                ]
            ]
        ]
    ]
  where
    btn cls label action =
      H.button_ [ P.class_ ("btn " <> cls), H.onClick action ] [ label ]
----------------------------------------------------------------------------
