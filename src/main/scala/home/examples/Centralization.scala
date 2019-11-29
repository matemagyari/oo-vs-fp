package home.examples

object Centralization {

  trait Money
  trait Email
  case class Product(id: Int, name: String, price: Money)
  case class Order(userId: Int, products: Set[Int])

  trait EmailService {
    def send(email: Email)
  }
  trait PaymentService {
    def pay(userId: Int, amount: Money)
  }
  trait OrderRepository {
    def makeOrder(order: Order): Unit
  }
  trait ProductRepository {
    def price(id: Int): Product
  }

  class OrderService(
      productRepository: ProductRepository,
      orderRepository: OrderRepository,
      paymentService: PaymentService,
      emailService: EmailService) {

    def buy(productId: Int, userId: Int): Unit = {

      val product = productRepository.price(productId)
      paymentService.pay(userId, product.price)
      orderRepository.makeOrder(Order(userId, Set(productId)))

    }
  }
}
