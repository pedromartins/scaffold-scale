main = forever switch[if threshold ?switch
                      then cmd turnOnLight
                      else cmd turnOffLight]

