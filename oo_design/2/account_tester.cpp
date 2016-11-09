#include <iostream>
#include <iomanip>

#include "Account.hpp"
#include "SavingsAccount.hpp"
#include "CheckingAccount.hpp"

using namespace std;

int main(void) {
    Account account(500);
    SavingsAccount savingsAccount(500, 0.01);
    CheckingAccount checkingAccount(500, 1.0);

    cout << fixed << setprecision(2);

    account.credit(250);
    account.debit(10.50);
    cout << "account now has balance = " << account.getBalance() << endl;
    
    savingsAccount.credit(700);
    savingsAccount.debit(100.60);
    cout << "savingsAccount now has balance = " << savingsAccount.getBalance() << endl;
    double accruedInterest = savingsAccount.calculateInterest();
    savingsAccount.credit(accruedInterest);
    cout << "After 1 year, savingsAccount will accrue interest of " << accruedInterest;
    cout << " for a total balance of " << savingsAccount.getBalance() << endl;

    checkingAccount.credit(10);
    checkingAccount.debit(10);
    cout << "checkingAccount now has balance = " << checkingAccount.getBalance() << endl;

    return 0;
}
