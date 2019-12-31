# Trello

A REST API exercise for university. Provides Haskell bindings for the [Trello](trello.com) REST API using [aeson](http://hackage.haskell.org/package/aeson) and [http-conduit](https://hackage.haskell.org/package/http-conduit). 


## Usage

Requests are made in the `TRequest` monad. It provides the key and token for requests and hides the underlying IO.

```haskell
newtype TRequest a = TReqeust { unTRequest :: ReaderT TrelloLogin IO a } deriving (Functor, Applicative, Monad, MonadReader TrelloLogin, MonadIO, MonadThrow)
```

This example creates a card in the first list of the first board containing some attchments.
 
```haskell
main :: IO ()
main = do
    login <- fromJust . decode <$> BS.readFile "login.json"
    runTRequest login $ do
        board <- B.all <&> head >>= L.all <&> head
        newCard <- C.post board
        A.post newCard "https://github.com/JonasHoefer/trello"
        img <- A.post newCard "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/602px-Haskell-Logo.svg.png"
        C.put $ newCard { C.name = "Card from Haskell",  C.desc = "This Card was created using the Haskell bindings for the `Trello.com` Rest API", C.idAttachmentCover = Just . A.id $ img }
        C.label newCard "purple" (Just "Haskell")
        return ()
```
