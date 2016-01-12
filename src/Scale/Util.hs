module Scale.Util where

import Scale.Types

depreqToCaps :: DepReq -> [DepReq]
depreqToCaps (d `And` d') = (depreqToCaps d ++ depreqToCaps d')
depreqToCaps d = [d]
