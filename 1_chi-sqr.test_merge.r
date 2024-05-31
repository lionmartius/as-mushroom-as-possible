# What do we need for successful woodland expansion?
# - As Mushroom as possible
# Ectomycorrhizal grove formation and implications for woodland expansion 
# Merged Dataset - EcM Group Formation around Salix 
# Lion R. Martius
# Edinburgh, 25/03/2024
# Chi-sqr test

# Chi-sqr test for likelihood of individual to be found as isolated individual 
# or in a group of EcM plants

# Betula
chisq.test(c(2, 261))

# Pine 
chisq.test(c(0, 33))

# Salix repens
chisq.test(c(49, 390))

# Salix aurita
chisq.test(c(17, 124))

# Salix spp. 
chisq.test(c(14, 42))


# Chi-sqr test for likelihood of equal numbers for Salix individual to be found
# within monospecific, monogeneric or mixed genus group

# Salix repens
chisq.test(c(88, 87, 215))

# Salix aurita
chisq.test(c(10, 35, 79))

# Salix spp.
chisq.test(c(2, 18, 22))


# Likelihood for All Salix to be in an exclusive Salix-only group or mixed genus
chisq.test(c(240, 316))

# Likelihood for All Salix to be in monospecific or mixed species group
chisq.test(c(456, 100))
