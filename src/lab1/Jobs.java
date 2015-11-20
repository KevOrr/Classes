package lab1;

/*
Program:    Jobs.java
Student:    Kevin Orr
Desc:       This program outputs 3 entry level jobs, necessary skills for each job,
            and documents the source for each listing
*/

public class Jobs {
    public static void main(String[] args) {

        // http://symbyo.catsone.com/careers/index.php?m=portal&a=details&jobOrderID=211517
        System.out.print("Job 1: Android Developer\n"
                        +"Required Skils:\n"
                        +"    BS in Computer Science, MIS, business, or equivalent education/training/experience\n"
                        +"    Strong knoweledge of at least one programming language\n"
                        +"    Must have a passion for learnining new programming languages and creating fun, user friendly applications\n"
                        +"    Experience building mobile application on any platform is a plus\n"
                        +"    OpenGL is a plus\n"
                        +"    Experience programming casual games is a plus\n\n");
        
        // http://www.careerarc.com/job-listing/at-t-jobs-member-of-technical-staff-at-t-net-bond-cloud-computing-16390378?src=2&utm_campaign=at&utm_medium=xml&utm_source=indeed
        System.out.print("Job 2: Member of Technical Staff AT&T Net Bond Cloud Computing\n"
                        +"Required Skills:\n"
                        +"    BS in CS or related major from approved university\n"
                        +"    Technical skils in Java coding on a Linux platform\n"
                        +"    HTML development and XML interfaces/APIs\n"
                        +"    Knowledge of socket programming is a plus\n\n");
        
        // http://www.internships.com/computer-science/Entry-level-Java-Developer-position?utm_source=Indeed&utm_medium=referral&utm_campaign=idc
        System.out.println("Job 3: Software Engineer/System Engineer\n"
                          +"Required Skills:\n"
                          +"    Recent graduation from the fields of CS, CE, EE, Software, MIS, IT, or other related engineering field\n");
    }
}
