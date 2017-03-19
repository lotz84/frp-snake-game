{-# LANGUAGE RecursiveDo #-}

import Control.Monad.Fix
import Data.List
import Gloss.FRP.Reactive.Banana (playReactive, InputEvent)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import Reactive.Banana.Combinators
import qualified Reactive.Banana.Combinators as FRP
import Reactive.Banana.Frameworks
import qualified System.Random.MWC as Random

-- reactive-bananaのための便利関数
-- 開始からn個のEventを通しそれ以降は捨てる
takeE :: Int -> FRP.Event a -> MomentIO (FRP.Event a)
takeE n event = do
  bCount <- accumB 0 ((+1) <$ event)
  pure $ fmap snd $ filterE (\(c,_) -> c < n) ((,) <$> bCount <@> event)

-- ゲーム上の位置を表す
type Position = (Int, Int)

-- ターゲットは一つの位置で表される
type Target = Position

-- 蛇は自身の長さと全ての節のゲーム上の位置を持つ
data Snake = Snake
  { _length :: Int
  , _body   :: [Position]
  }

-- 蛇が取れる行動
data SnakeAction = Stop | MoveUp | MoveDown | MoveLeft | MoveRight deriving Show

-- ゲームの状態は蛇とターゲットの組で表される
data GameState = GameState
  { _snake  :: Snake
  , _target :: Target
  }

-- シーンを表す
data GameScene = TitleScene | MainGame deriving Show

-- シーンの実装の型
type GameSceneHandler =  Random.GenIO                -- 乱数のシード
                      -> Handler GameScene           -- シーンを遷移するための関数（一回しか呼んではならない。乱用禁止！）
                      -> FRP.Event Float             -- 1フレームごとに発火するイベント
                      -> FRP.Event InputEvent        -- 外部入力ごとに発火するイベント
                      -> MomentIO (Behavior Picture) -- 実行結果として描画する画面

-- 蛇の初期状態
initialSnake :: Snake
initialSnake = Snake 1 [(10, 10)]

-- キーボードからの入力から蛇の行動への変換
event2Action :: InputEvent -> Maybe SnakeAction
event2Action (EventKey (SpecialKey KeyUp)    Down _ _) = Just MoveUp
event2Action (EventKey (SpecialKey KeyDown)  Down _ _) = Just MoveDown
event2Action (EventKey (SpecialKey KeyLeft)  Down _ _) = Just MoveLeft
event2Action (EventKey (SpecialKey KeyRight) Down _ _) = Just MoveRight
event2Action _                                         = Nothing

-- glossからのEventがキーボード入力かどうか判定する
isKeyEvent :: InputEvent -> Bool
isKeyEvent (EventKey (Char _)       _ _ _) = True
isKeyEvent (EventKey (SpecialKey _) _ _ _) = True
isKeyEvent _                               = False

-- 蛇の行動が可能かどうかを判定する関数
-- 蛇は現在の進行方向と真反対には動けない
selectAction :: SnakeAction -> SnakeAction -> SnakeAction
selectAction MoveUp    MoveDown  = MoveUp
selectAction MoveDown  MoveUp    = MoveDown
selectAction MoveLeft  MoveRight = MoveLeft
selectAction MoveRight MoveLeft  = MoveRight
selectAction prev      next      = next

-- 蛇の行動と新しい長さを元に蛇を更新する関数
updateSnake :: SnakeAction -> Int -> Snake -> Snake
updateSnake Stop _ xs = xs
updateSnake MoveUp    l (Snake _ body@((x,y):_)) = Snake l $ take l ((x, y + 1):body)
updateSnake MoveDown  l (Snake _ body@((x,y):_)) = Snake l $ take l ((x, y - 1):body)
updateSnake MoveLeft  l (Snake _ body@((x,y):_)) = Snake l $ take l ((x - 1, y):body)
updateSnake MoveRight l (Snake _ body@((x,y):_)) = Snake l $ take l ((x + 1, y):body)

-- 蛇が自己交差を持つか判定する関数
isSelfIntersecting :: Snake -> Bool
isSelfIntersecting snake =
  let b = _body snake
   in length (nub b) /= length b

-- ゲーム上の一マスを描画する関数
tile :: Color -> Position -> Picture
tile c (x, y) =
  let (x', y') = (fromIntegral x * 10, fromIntegral y * 10)
   in translate x' y' $ color c $ rectangleSolid 10 10

-- 蛇を描画する関数
drawSnake :: Snake -> Picture
drawSnake snake = pictures $ map (tile white) (_body snake)

-- ターゲットを描画する関数
drawTarget :: Target -> Picture
drawTarget = tile red

-- ゲームの状態を描画する関数
drawGameState :: GameState -> Picture
drawGameState gs = pictures [drawSnake (_snake gs), drawTarget (_target gs)]

-- ランダムなターゲットの位置を取得する関数
genTargetPos :: MonadIO io => Random.GenIO -> io Position
genTargetPos gen = do
  x <- liftIO $ Random.uniformR (-32, 32) gen
  y <- liftIO $ Random.uniformR (-24, 24) gen
  pure (x, y)

-- ゲームのシーンからその処理関数を取得する
getHandler :: GameScene -> GameSceneHandler
getHandler TitleScene = bTitleScene
getHandler MainGame   = bMainGame

-- タイトル画面
bTitleScene :: GameSceneHandler
bTitleScene _ sceneHandler eTick eEvent = do
  let title       = translate (-200) 0     $ scale 0.5  0.5  $ color red   $ text "Snake Game"
      description = translate (-140) (-30) $ scale 0.15 0.15 $ color white $ text "Please press any key to start."

  -- 何らかのキーが押されたらゲームを開始する
  eAnyKeyPressed <- takeE 1 $ pure isKeyEvent `filterApply` eEvent
  reactimate $ (liftIO $ sceneHandler MainGame) <$ eAnyKeyPressed

  pure $ (pure (pictures [title, description]))

-- ゲーム画面
bMainGame :: GameSceneHandler
bMainGame gen sceneHandler eTick eEvent = do
  -- ゲームを実行する間隔を制御するための Event
  eTickCount <- accumE 0 $ fmap (const (+1)) eTick
  let eGameStep = () <$ ((\x -> x `mod` 5 == 0) `filterE` eTickCount)

  -- キーボードから入力された蛇の行動を表す Behavior
  let eActionEvent = filterJust $ fmap event2Action eEvent
  bSnakeAction <- mfix $ \b -> stepper Stop (selectAction <$> b <@> eActionEvent)

  -- 蛇とターゲットのBehavior
  initial <- genTargetPos gen
  (bSnake, bTarget) <- mdo
    -- 蛇がターゲットを捉える Event
    let eCatch = whenE (elem <$> bTarget <*> (fmap _body bSnake)) eGameStep

    -- ターゲットは蛇に捉えられる毎に移動する
    eTargetPos <- execute $  genTargetPos gen <$ eCatch
    bTarget <- stepper initial eTargetPos

    -- 蛇はターゲットを捉える毎に長くなる
    bSnakeLength <- accumB 1 $ (+1) <$ eCatch
    bSnake <- accumB initialSnake (updateSnake <$> bSnakeAction <*> bSnakeLength <@ eGameStep)

    pure (bSnake, bTarget)

  -- 蛇が自己交差を持てばタイトル画面に移動する
  eGameOver <- takeE 1 $ filterE id $ fmap isSelfIntersecting bSnake <@ eGameStep
  reactimate $ (liftIO $ sceneHandler TitleScene) <$ eGameOver

  pure $ fmap drawGameState (GameState <$> bSnake <*> bTarget)

main :: IO ()
main = do
  gen <- Random.createSystemRandom
  let window = InWindow "Snake Game" (640, 480) (100, 100)
  playReactive window black 60 $ \eTick eEvent -> do
    (eScene, sceneHandler) <- newEvent
    bScene <- execute $ fmap (\s -> getHandler s gen sceneHandler eTick eEvent) eScene
    initial <- bTitleScene gen sceneHandler eTick eEvent
    switchB initial bScene
