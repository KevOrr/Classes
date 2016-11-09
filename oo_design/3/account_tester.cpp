#include <iostream>
#include <iomanip>
#include <vector>

#include "Account.hpp"
#include "SavingsAccount.hpp"
#include "CheckingAccount.hpp"

using namespace std;

int main(void) {
    cout << fixed << setprecision(2);

    vector<Account*> container;

    for (int i = 0; i < 2; i++)
        container.push_back(new SavingsAccount(1000 * (i + 1), 0.01));

    for (int i = 0; i < 2; i++)
        container.push_back(new CheckingAccount(200 * (i + 1), 5));

    for (vector<Account*>::iterator it = container.begin(); it != container.end(); ++it) {
        double debit;
        double credit;

        cout << "How much would you like to withdraw: ";
        cin >> debit;
        cout << "How much would you like to deposit: ";
        cin >> credit;

        (*it)->debit(debit);
        (*it)->credit(credit);

        if (dynamic_cast<SavingsAccount*>(*it) != NULL)
            (*it)->credit(dynamic_cast<SavingsAccount*>(*it)->calculateInterest());

        cout << "New balance is " << (*it)->getBalance() << endl;

    }

    return 0;
}
