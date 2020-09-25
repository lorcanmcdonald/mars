module Mars.Renderable where
import Data.Text

class Renderable a where
  render :: a -> Text

