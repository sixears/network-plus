{-| A simple collection of high-level network tools -}
module NetworkPlus
  ( lanIPs, wanIP )
where

import Base1

-- base --------------------------------

import Data.Bits  ( (.&.), shiftR )

-- base-unicode-symbols ----------------

import Prelude.Unicode  ( (≠) )

-- duration ----------------------------

import Duration  ( Duration( SECS ) )

-- http-plus ---------------------------

import HTTPPlus  ( httpRequest )

-- ip4 ---------------------------------

import IP4 qualified
import IP4  ( IP4 )

-- network-info ------------------------

import Network.Info  qualified as  NI
import Network.Info  ( getNetworkInterfaces )

-- parsec-plus-base --------------------

import Parsec.Error  ( AsParseError )

--------------------------------------------------------------------------------

niIPv4toIP4 ∷ NI.IPv4 → IP4
niIPv4toIP4 (NI.IPv4 ni4) =
  let (a,b,c,d) = ( fromIntegral (ni4 `shiftR` 24)
                  , fromIntegral ((ni4 `shiftR` 16) .&. 0xFF)
                  , fromIntegral ((ni4 `shiftR` 8)  .&. 0xFF)
                  , fromIntegral (ni4 .&. 0xFF)
                  )
  in  IP4.fromOctets d c b a

----------------------------------------

{-| IPs as shown to the LAN; excluding loopback and unassigned interfaces -}
lanIPs ∷ MonadIO μ ⇒ μ [IP4]
lanIPs =
  liftIO $ niIPv4toIP4 ⊳⊳
      (getNetworkInterfaces ⊲ \ nis → [ NI.ipv4 ni | ni ← nis
               {- exclude lo                    -} , NI.name ni ≠ "lo"
               {- exclude unassigned interfaces -} , NI.ipv4 ni ≠ NI.IPv4 0 ])

----------------------------------------

{-| WAN IP, if any -}
wanIP ∷ (MonadIO μ, AsIOError ε, AsParseError ε, MonadError ε μ) ⇒ μ (𝕄 IP4)
wanIP = httpRequest @_ @IP4 (SECS 2) "http://whatismyip.akamai.com"

-- that's all, folks! ----------------------------------------------------------
