namespace Numbers

Module "Record" : Record { 
  Functor "RecordType" : *
}

Functor "EmptyRecord" : Record
Functor "RecordField" => string => * => Record : Record

Func "foo" -> int -> r.RecordType : RecordType

-----------------
EmptyRecord => Record {
  
  Func "cons" : unit

  ------------------
  RecordType => int
  
}
