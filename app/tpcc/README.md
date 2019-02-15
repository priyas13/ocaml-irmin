## TPCC
- TPCC can be used for portraying the activity of a wholesale supplier.
Suppose there is a wholesale supplier with a number of geographically distributed sale districts and associated warehouses. As company expand, new warehouse and associated sales districts are created. Hence there are various types defined to represent the wholesale supplier. 
- New order transaction:
  - It places a new order for a customer for the list of items he wants.
  - This order needs to be served under a particular district under a particular warehouse.
  - After a new order transaction, a new order record is added to the list of new orders whose order id is equal to the 
    next order id in the district record. (district should be responsible for dispatching this order). Its id must be the 
    sequence number of the next order under the given district. 
  - An order is created for the new order whose count is equal to the number of items requested for that order. This order is 
    added to the order table.
  - Also the district next order is being increased by 1.
  - Also for each item an order_line record is created which is added to the order line table.
  - Each item has the corresponding stock in the list of stock which keeps the track of the quantity of the item left in the       stock. If the stock quantity is less than the number of items requested by customer then the stock quantity is replenished     by 100. 
  - Stock is being updated after the new order transaction which reflects the changes in its quantity. If the new order
    request of the customer is being processed than the stock quantity is decreased by that amount.
