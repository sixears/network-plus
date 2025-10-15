module NetworkPlus
  ( LanIPs( unLanIPs ), wanIP )
where

--------------------------------------------------------------------------------

----------------------------------------

newtype LanIPs = LanIPs { unLanIPs ∷ [NI.IPv4] }
  deriving Show

lanIPs ∷ MonadIO μ ⇒ μ LanIPs
lanIPs = liftIO $ LanIPs ⊳
  (getNetworkInterfaces ⊲ \ nis → [ NI.ipv4 ni | ni ← nis
           {- exclude lo                    -} , NI.name ni ≠ "lo"
           {- exclude unassigned interfaces -} , NI.ipv4 ni ≠ NI.IPv4 0 ])

----------------------------------------

wanIP ∷ MonadIO μ ⇒ μ 𝕋
wanIP =
  let url     = "http://whatismyip.akamai.com"
  in  ѥ (httpRequest @ScriptError @IP4 (SECS 2) url) ⊲ \ case
        𝓛 _e    → "-ERR- " -- ◇ T.take 8 (toText e)
        𝓡 (𝓙 r) → toText r
        𝓡 𝓝     → "NONE"

-- `ip monitor -tshort address` to see addresses come & go
-- output e.g.,
-- [2025-10-09T12:42:14.472565] Deleted 2: wlp0s20f3    inet 192.168.0.10/24 brd 192.168.0.255 scope global noprefixroute wlp0s20f3
-- [2025-10-09T12:42:18.677739] 2: wlp0s20f3    inet 192.168.0.10/24 brd 192.168.0.255 scope global noprefixroute wlp0s20f3
lanWanIPs ∷ MonadIO μ ⇒ μ [𝕋]
lanWanIPs = do
  lan_ips ← lanIPs
  wan_ip ← case lan_ips of
             LanIPs [] → return ""
             _         → wanIP

  let lan_ips_str = case unLanIPs lan_ips of
                      []  → "NONE"
                      ips → T.intercalate "," (T.pack ∘ show ⊳ ips)
  return $ "ⓛ " ◇ lan_ips_str : case wan_ip of "" → []; _ → ["ⓦ " ◇ wan_ip]
-- XXX don't even try if there is no route?
-- XXX just drop this if there is no wan_ip
--         , "ⓦ " ◇ (wan_ip {- ⧏ "UNKNOWN" -})
--         ]


-- that's all, folks! ----------------------------------------------------------
