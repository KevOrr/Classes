#include "dunningkruger.h"

#include <random>
#include <cmath>
using namespace std;

Subject::Subject() {
    competence = -1;
}

Subject::Subject(double comp) {
    competence = -1;
    setCompetence(comp);
}

void Subject::setCompetence(double comp) {
    if (competence != -1)
        return;

    competence = comp;
    if (competence < 0)
        competence = 0;
    else if (competence > 100)
        competence = 100;
}

double Subject::getCompetence() const
{
    return competence;
}

bool Subject::considersThemselfBetterThan(const Subject& oth) const
{
    return getBelief() > oth.competence;
}

double Subject::getBelief() const
{
    return competence;
}

double SlightlyOff::getBelief() const
{
    double belief = competence;

    //Illusory superiority
    if (competence < LOW_COMPETENCE)
        belief = 1.01 * LOW_COMPETENCE - 0.01 * competence + 1.5;

    //Illusory inferiority
    if (competence >= HI_COMPETENCE)
        belief = belief - 1.5;

    return belief;
}

double Delusional::getBelief() const
{
    double belief = competence;

    //Illusory superiority
    if (competence < LOW_COMPETENCE)
        belief = 3 * LOW_COMPETENCE - 2 * competence + 5.5;

    //Illusory inferiority
    if (competence >= HI_COMPETENCE)
        belief = competence / 2 + LOW_COMPETENCE - 0.25;

    return belief;
}

bool Subject::operator<(const Subject& oth) const
{
    return !considersThemselfBetterThan(oth);
}

bool Subject::operator>(const Subject& oth) const
{
    return considersThemselfBetterThan(oth);
}

inline void swap(Subject& a, Subject& b)
{
    double t = a.competence;
    a.competence = b.competence;
    b.competence = t;
}

void shuffle(Subject* arr, int n)
{
    for (int i = 0; i < n - 1; i++)
    {
        int idx = rand() % (n - i);
        swap(arr[i], arr[idx]);
    }
}

double normcdf(double x)
{
    static const double a1 = 0.254829592;
    static const double a2 = -0.284496736;
    static const double a3 = 1.421413741;
    static const double a4 = -1.453152027;
    static const double a5 = 1.061405429;
    static const double p = 0.3275911;

    x /= sqrt(2.0);

    // A&S formula 7.1.26
    double t = 1.0 / (1.0 + p*x);
    double y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);

    return 0.5*(1.0 + y);
}

double score(Subject* arr, int n)
{
    unsigned long long count = 0;
    for (int i = 0; i < n; i++)
        for (int j = i + 1; j < n; j++)
            if (arr[i].competence >= arr[j].competence)
                count++;

    return 2 - 2*normcdf(count * 6.0 / n / (n+1));
}
