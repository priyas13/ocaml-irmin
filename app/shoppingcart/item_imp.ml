
module Item = 
 struct 
  module OC = Counter.Make 
  type t = {item_id : string; item_quantity : OC.t}
  let merge3 ancestor v1 v2 = if ((ancestor.item_id = v1.item_id) && (v1.item_id = v2.item_id) && (ancestor.item_id = v2.item_id)) 
                              then {item_id = ancestor.item_id ; item_quantity = OC.merge (ancestor.item_quantity) (v1.item_quantity) (v2.item_quantity)} 
                              else failwith "Merge not possible"
end 