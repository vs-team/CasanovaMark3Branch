namespace Numbers

Module "Record" : Record { 
  Functor "RecordType" : *
}

Module "Getter" => (name : string) => (r : Record) : Getter {
  Functor "GetType" : *
  Func "get" -> r.RecordType : GetType
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

name = fieldName
Recordfield name type r => thisRecord
--------------------------------------------
GetField fieldName (RecordField name type r) => Getter fieldName thisRecord {
  
  ----------------
  GetType => (type)

  --------------
  get (x,xs) -> x
  
}

name <> fieldName
Recordfield name type r => thisRecord
--------------------------------------
GetField fieldName (RecordField name type r) => Getter fieldName type thisRecord {
  
  Functor "GetAnotherField" : Getter

  GetField fieldName r => otherGetter
  -----------------------------------
  GetAnotherField => (otherGetter)

  GetAnotherField => g
  g.GetType => type
  --------------------
  GetType => (type)

  GetAnotherField => getter
  getter.get xs -> v
  -------------------------
  get (x,xs) -> v

}