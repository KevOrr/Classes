clean:
	rm *.o account_tester || true

account_tester: account_tester.o Account.o SavingsAccount.o CheckingAccount.o
	g++ -o account_tester account_tester.o Account.o SavingsAccount.o CheckingAccount.o

account_tester.o: account_tester.cpp
	g++ -c account_tester.cpp

Account.o: Account.cpp Account.hpp
	g++ -c Account.cpp

SavingsAccount.o: SavingsAccount.cpp SavingsAccount.hpp
	g++ -c SavingsAccount.cpp

CheckingAccount.o: CheckingAccount.cpp CheckingAccount.hpp
	g++ -c CheckingAccount.cpp

