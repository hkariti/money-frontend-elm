module Account exposing (Account)


type alias Account =
    { id : Int
    , name : String
    , backend_id : String
    , backend_type : String
    }
