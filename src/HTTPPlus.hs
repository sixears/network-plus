module HTTPPlus
  ( httpReq, httpRequest )
where

import Base1

import Prelude  ( round )

-- base --------------------------------

import Control.Exception  ( Handler( Handler ), SomeException
                          , catches, displayException )
import Data.Char          ( isAscii, isControl )
import Data.List          ( take, takeWhile )
import System.Timeout     ( timeout )

-- bytestring --------------------------

import Data.ByteString.Lazy  qualified as LBS

-- duration ----------------------------

import Duration  ( Duration, asMicroseconds )

-- http-client -------------------------

import Network.HTTP.Client           ( httpLbs, newManager, parseRequest
                                     , responseBody )
import Network.HTTP.Client.Internal  ( HttpException( HttpExceptionRequest
                                                    , InvalidUrlException ) )
import Network.HTTP.Client.TLS       ( tlsManagerSettings )


-- monaderror-io -----------------------

import MonadError.IO.Error  ( throwUserError )

-- parsec-plus -------------------------

import ParsecPlus  ( AsParseError, Parsecable, parsec )

-- text --------------------------------

import Data.Text  qualified as  T

import Data.Text.Encoding  ( decodeUtf8 )

--------------------------------------------------------------------------------

isSimpleAscii ∷ ℂ → 𝔹
isSimpleAscii c = isAscii c ∧ ﬧ  (isControl c)

----------------------------------------

{-| Issue an HTTP request, with a given timeout.  If no response is received
    within the time allowed, 𝓝 is returned -}
httpReq ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ, HasCallStack) ⇒
          Duration → 𝕋 → μ (𝕄 𝕋)
httpReq timeout_ url =
  let catcher io =
        let some_ex_h (e∷SomeException) =
              let take_take      = take 20 ∘ takeWhile isSimpleAscii
              in  ѥ (throwUserError ∘ take_take $ displayException e)

            http_ex_h (e∷HttpException) =
              case e of
                HttpExceptionRequest _ ex → return $ throwUserError $ show ex
                InvalidUrlException _ ex  → return $ throwUserError ex

        in  catches io [ Handler http_ex_h, Handler some_ex_h ]
  in  join ∘ liftIO ∘ catcher ∘ ѥ ∘ asIOError $ do
        manager ← newManager tlsManagerSettings
        request ← parseRequest (T.unpack url)
        timeout (round $ timeout_ ⊣ asMicroseconds) $ do
          response ← httpLbs request manager
          return ∘ decodeUtf8 ∘ LBS.toStrict $ responseBody response

--------------------

{-| see `httReq`; but then attempt to parse the returned text -}
httpRequest ∷ ∀ ε α μ .
              (MonadIO μ, AsParseError ε, AsIOError ε, MonadError ε μ,
               Parsecable α) ⇒
              Duration → 𝕋 → μ (𝕄 α)
httpRequest timeout_ url = do
  html ← httpReq timeout_ url
  case html of
    𝓝 → return 𝓝
    𝓙 t → case parsec t t of
            𝓛 e → join $ throwError e
            𝓡 r → return $ 𝓙 r

-- that's all, folks! ----------------------------------------------------------
