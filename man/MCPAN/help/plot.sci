plot.sci                package:MCPAN                R Documentation

_P_l_o_t _s_i_m_u_l_t_a_n_e_o_u_s _c_o_n_f_i_d_e_n_c_e _i_n_t_e_r_v_a_l_s

_D_e_s_c_r_i_p_t_i_o_n:

     Creates a high level plot of confidence intervals from objects
     returned by functions binomRDci, binomORci, poly3ci. Plot function
     for objects of class "sci".

_U_s_a_g_e:

     ## S3 method for class 'sci':
     plot(x, line = 0, linelty = 2, linelwd = 1, linecol = "black", CIvert = FALSE, CIlty = 1, CIlwd = 1, ...)
     ## S3 method for class 'binomRDci':
     plot(x,...)
     ## S3 method for class 'poly3ci':
     plot(x,...)
     ## S3 method for class 'binomORci':
     plot(x, ...)

_A_r_g_u_m_e_n_t_s:

       x: an object of class "sci", "binomRDci", "poly3ci", as can be
          obtained by calling 'binomRDci','poly3ci'            

    line: as single numeric value or vector, position of line(s) to be
          added orthogonal to the plotted CI 

 linelty: line type(s) for 'line' as defined in 'par' 

 linelwd: line width(s) for 'line' as defined in 'par'  

 linecol: colour(s) for 'line' as defined in 'par'  

  CIvert: logical value, specifying whether confidence intervals shall
          be represented by vertical (if TRUE) or horizontal (if FALSE)
          lines 

   CIlty: single value, line type for confidence intervals as defined
          in 'par'  

   CIlwd: single value, line type for confidence intervals as defined
          in 'par'  

     ...: parameters to be passed to 'plot.default', as documented in
          'plot.default', 'plot', or 'par'  

_D_e_t_a_i_l_s:

     Currently, the 'plot' parameters 'ylim, xlim, las, axes, type,
     pch' and the 'axis' parameters 'labels, las' are chosen internally
     by the function and can not be set by the user.  Currently,
     combining several plots via 'par("mfrow")' or 'layout' in one
     device is not possible.

_V_a_l_u_e:

     A plot.

_A_u_t_h_o_r(_s):

     Frank Schaarschmidt

_R_e_f_e_r_e_n_c_e_s:

_E_x_a_m_p_l_e_s:

     # An example with three dose groups
     # compared to Placebo, binomial response:

     resp<-c(2,8,6,13)
     trials<-c(34,33,36,34)
     names(resp)<-c("Placebo", "50", "75", "150")

     ci<-binomRDci(x=resp, n=trials,
      type="Dunnett", method="ADD1")
     plot(ci, line=c(0,0.1,0.2),linelty=c(1,2,3))

