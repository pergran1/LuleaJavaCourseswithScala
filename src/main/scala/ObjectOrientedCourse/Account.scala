package ObjectOrientedCourse

import ObjectOrientedCourse.Account.Account.newUniqueNumber

object Account extends  App{


  case class Account(var balance: Double = 0, interest: Double = 0.012, accountType: String = "Sparkonto"){

    //Increase by 1
    val accountNumber = newUniqueNumber()

    def deposit(amount: Int): Boolean = {
      if(amount > 0){
        balance += amount
        return true
      }
      false
      }

    def withdraw(amount: Int): Boolean = {
      if (amount > 0 && balance >= amount){
        balance -= amount
        return true
      }
      false
      }

    override def toString: String = s"$accountNumber $balance $accountType $interest"

  }



  object Account{
    // get unique for each account
    private var lastNumber = 1001
    def newUniqueNumber() = {lastNumber += 1; lastNumber}
  }

  //--------------Customer class-------------------------------------------------------------------------------

  case class Customer(var name: String, var surName: String, var pNo: String){

    var accountList: List[Account] = List()
    def createAccount(): Int = {
      val newAcc: Account = new Account()
      accountList = accountList :+ newAcc
      newAcc.accountNumber
    }

    def changeName(newName: String, newSurName: String): Unit ={
      if(newName.isEmpty == false) name = newName
      if (newSurName.isEmpty == false) surName = newSurName

    }

    def findAccount(accNumber: Int): Account ={
      val foundAccount = accountList.find(_.accountNumber == accNumber)
      foundAccount match {
        case Some(x) => x
        case None => null
      }
    }

    override def toString: String = s"$pNo $name $surName"

  }

  // -----BankLogic ---------------
  case class Banklogic(){
    // Methods for creating and deleting customers/accounts
    var customerList: List[Customer] = List()

    def getAllCustomers(): List[String] = {
      customerList.map(_.toString) //calls customers toString on all customers
    }


    def createCustomer(name: String, surName: String, pNo: String):Boolean = {
      val customer: Customer = findCustomer(pNo)
      // If customer is null then the customer does not exist and we can create it
      if(customer == null) {
        customerList = customerList :+ new Customer(name, surName, pNo)
        true
      } else {
        false
      }

    }

    def getCustomer(pNo: String): List[String] = {
      // First find out if there is a customer
      // if there is a customer then I will create a list with info
      val foundCustomer: Customer = findCustomer(pNo)
      if (foundCustomer != null){
        // the customer exist
        var infoList: List[String] = List()
        infoList = infoList :+ foundCustomer.toString
        foundCustomer.accountList.foreach(account => infoList =  infoList :+ account.toString)
        return infoList
      }
      return null
    }

    def changeCustomerName(name: String, surName: String, pNo: String): Boolean = {
      val foundCustomer = findCustomer(pNo)
      if (foundCustomer != null) {
        foundCustomer.changeName(name, surName)
        return true
      }
      false
    }

    def createSavingsAccount(pNo: String): Int = {
      val foundCustomer: Customer = findCustomer(pNo)
      if (foundCustomer != null) {
        val  accountNumber = foundCustomer.createAccount()
        return accountNumber
      }
      -1
    }

    def getAccount(pNo: String, accountNumber: Int): String = {
      val foundCustomer: Customer = findCustomer(pNo)
      if (foundCustomer != null){
        val foundAccount: Account = foundCustomer.findAccount(accountNumber)
        if (foundAccount != null){
          return foundAccount.toString
        } else {null}

      }else {null}
    }

    def deposit(pNo: String, accountNumber: Int, amount: Int): Boolean = {
      val foundAccount: Account = findAccount(pNo, accountNumber)
      if (foundAccount != null){
        return foundAccount.deposit(amount)
      }
      false

    }

    def withdraw(pNo: String, accountNumber: Int, amount: Int): Boolean = {
      val foundAccount: Account = findAccount(pNo, accountNumber)
      if(foundAccount != null){
        return foundAccount.withdraw(amount)
      }
      false
    }

    // get the account from a customer
    def findAccount(pNo: String, accountNumber: Int): Account ={
      val customer = findCustomer(pNo)
      if (customer != null){
        val account: Option[Account] = customer.accountList.find(_.accountNumber == accountNumber)
        return account match {
          case Some(x) => x
          case None => null
        }
      }
      null
    }

    def findCustomer(pNo: String): Customer = {
      // Controls if the customer exist
      //val result = customerList.filter(_.pNo == pNo)
      val result: Option[Customer] = customerList.find(_.pNo == pNo)
      // find results in a option and I need to pick that option out
      result match {
        case Some(x) => x
        case None => null
      }
    }
  }






  val testar: Account = new Account()
  val testar2: Account = new Account()

  val cust1: Customer = new Customer("Per", "Granberg", "27")
  println(s"Customer is $cust1")
  println(s"Created accoutn:; ${cust1.createAccount()}")
  println(s"Created accoutn:; ${cust1.createAccount()}")
  println(s"Listan är ${cust1.accountList}")


  println(testar.deposit(23))

  println(testar.withdraw(4))
  //println(testar.balance)
  println(testar.accountNumber)
  println(testar2.accountNumber)

  println("Banklogic starts")
  val bank = new Banklogic()
  bank.createCustomer("Pelle", "Svensson", "943")
  bank.createCustomer("Pelle4", "Svensson", "943")
  println(bank.findCustomer("943"))
  println(bank.createCustomer("Nils", "Svensson", "932"))
  println(bank.createCustomer("Nils", "Svensson", "922"))
  println(s"Customerlistan är: ${bank.customerList}")

  println(bank.getCustomer("943"))
  bank.createSavingsAccount("943")
  println(bank.createSavingsAccount("943"))
  println(bank.changeCustomerName("PelleNew", "", "943"))
  println(bank.deposit("943", 1006, 150 ))
  println(bank.getCustomer("943"))
  println(bank.getAllCustomers())
  println(bank.withdraw("943", 1006, 30 ))
  println(bank.getAccount("943", 1006))





}
