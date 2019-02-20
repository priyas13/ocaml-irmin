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
  - Each item has the corresponding stock in the list of stock which keeps the track of the quantity of the item left in the       stock. If the stock quantity is less than the number of items requested by customer then the stock quantity is replenished     by 91. 
  - Stock is being updated after the new order transaction which reflects the changes in its quantity. If the new order
    request of the customer is being processed than the stock quantity is decreased by that amount.
- Payment transaction:
  - It updates warehouse, district and customer record after the payment by the customer is made.
  - So after a payment is made by the customer, the customer's balance field is decremented by the payment amount and 
    his year to date payment field is being incremented by that amount.
  - Also the corresponding warehouse and district fields are updated to reflect changes in the year to date balance. 
  - This payment is also added to the history list.
- Delivery transaction:
  - It resembles the delivery that is done under a warehouse id. Processing batch of 10 new orders which are not 
    yet delivered.
  - It processes orders corresponding to 10 pending orders, one for each district, with 10 items per order.
  - First we get all the districts that come under that particular warehouse id. 
  - Then we keep track of all the new orders takes care under that district. 
  - The corresponding entry in the new order table is deleted.
  - We select the order with order id as same as lowest new order id and update it in the order list.
