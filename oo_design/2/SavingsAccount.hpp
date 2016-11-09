#ifndef __SAVINGSACCOUNT_H_
#define __SAVINGSACCOUNT_H_

#include "Account.hpp"

class SavingsAccount : public Account {
    protected:
        double interest;
    public:
        SavingsAccount(double initial_deposit, double interest);
        double getInterest();
        double calculateInterest();
};

#endif

