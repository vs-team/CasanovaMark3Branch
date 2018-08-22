namespace Numbers

Module "Record" : Record { 
  Functor "RecordType" : *
}

Module "Getter" => (name : string) => (r : Record) : Getter {

}

Functor "EmptyRecord" : Record
Functor "RecordField" => string => * => Record : Record
Functor "GetField" => string => Record : Getter

-----------------
EmptyRecord => Record {
  
  Func "cons" : unit

  ------------------
  RecordType => (unit)
  
}


-----------------------
RecordField name type r => Record {

  Func "cons" -> type -> r.RecordType : RecordType

  --------------------------------
  RecordType => (Tuple[type,r.RecordType])

  ---------------------
  cons x xs -> (x,xs)
  
}