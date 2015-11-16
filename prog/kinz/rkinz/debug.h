// -*- c++ -*-

#ifndef DEBUG_H
#define DEBUG_H 1

#ifndef NDEBUG
#include <iostream>
#define DBGP(x) std::cout << __FILE__ ":" << __LINE__ << " " << __FUNCTION__ << x << std::endl
#else  /* !NDEBUG */
#define DBGP(x) 
#endif /* !NDEBUG */
#define DBGL DBGP("")
#define DBGV(x) DBGP(": " #x "='" << x << "'")

#endif /* DEBUG_H */
