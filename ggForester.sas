*======================================
%macro ggForester(
csv_path= fliename or path, 
fig_type= ,
effect_type= ,
fig_size= width*height,
xaxis_value= value list,
text_var=  variable list,
plot_var= variable list,
fig_save= filename or path, 
)

Contact: guhongqiu@yeah.net

Note:
1. CSV_PATH can be a filename or path
2. FIG_TYPE can be REG, SUB, EST, META, REG_MX, REG_MC, REG_MR, SUB_MX, SUB_MC, SUB_MR, EST_MX, EST_MC, EST_MR
3. EFFECT_TYPE can be OR, RR, HR, RD

Log:
1. 2024-08-13: Version 1.0

======================================;

%macro ggForester(
csv_path=, 
fig_type=, 
effect_type=,
fig_size=,
xaxis_value=,
text_var=,
plot_var=,
fig_save=, 
);


*===1.Data import and repare;

OPTIONS  OBS=2;
proc import out=varlbl datafile="&csv_path"
             dbms=csv replace;
			 getnames=no;
			 datarow=1;
run;

OPTIONS minoperator symbolgen mprint nofmterr OBS=max;
proc import out=vardat datafile="&csv_path"
             dbms=csv replace;
			 getnames=Yes;
			 datarow=3;
run;

data _null_;
    set varlbl end=last;
    array vars_first_row{*} _character_;
    array vars_second_row{*} _character_;

    if _n_ = 1 then do;
        do i=1 to dim(vars_first_row);
           call symputx(cats('vars_first_row',i), vars_first_row{i});
        end;
    end;
    if _n_ = 2 then do;
        do i = 1 to dim(vars_second_row);
			call symputx(cats('vars_second_row',i), vars_second_row{i});
     end;
    end;
run;


proc contents data=varlbl out=var_info(keep=name) noprint;
run;

proc sql noprint;
    select count(*) into :num_vars from var_info;
quit;


proc datasets lib=work noprint;
	modify vardat;
	label %do i=1 %to &num_vars; 
            &&vars_first_row&i="&&vars_second_row&i" 
	     %end;;
quit;

%if %upcase(&fig_type) in %str(REG_MR SUB_MR EST_MR) %then %do;
 proc sql;
    create table rw as
    select distinct group as rwlbl
    from vardat(where=(not missing(group)));
 quit;

 data _null_;
   set rw end=last;
   call symputx(cats('rwlbl',_n_),rwlbl);
   if last then call symputx('nrwlbl',_n_);
 run;

%end;

*===2. Common paramters settings;

*=Effect type;
%if %quote(&effect_type) EQ %then %do;
    %let effect_type1=OR;
    %let effect_type2=RD;
%end;
%else %do;
   %let effect_type1=%scan(&effect_type,1, |);
   %let effect_type2=%scan(&effect_type,2, |);
%end;

*=Refvale,xaxis_type1;

*=Refvale,xaxis_type1;
%if %index(%upcase(&effect_type1), OR) or %index(%upcase(&effect_type1), RR) or %index(%upcase(&effect_type1), HR) %then %do;
    %let refvalue1=1;
    %let xaxis_type1=log;
    %let axisopt1=logopts;
%end;
%else %do;
    %let refvalue1=0;
    %let xaxis_type1=linear;
    %let axisopt1=linearopts;
%end;

%if %index(%upcase(&effect_type2), OR) or %index(%upcase(&effect_type2), RR) or %index(%upcase(&effect_type2), HR) %then %do;
    %let refvalue2=1;
    %let xaxis_type2=log;
    %let axisopt2=logopts;
%end;
%else %do;
    %let refvalue2=0;
    %let xaxis_type2=linear;
    %let axisopt2=linearopts;
%end;



*=Xaxis value;
%if &xaxis_value EQ %then %do;
   %if %index(%upcase(&effect_type1), OR) or %index(%upcase(&effect_type1), RR) or %index(%upcase(&effect_type1), HR) %then %do;
      %let xaxis_value1=0.25 0.5 1 2 4;
   %end; 
   %else %do;
       %let xaxis_value1=-5 -2.5 0 2.5 5;
   %end;

   %if %upcase(&fig_type) in %str(REG_MX REG_MC SUB_MX SUB_MC EST_MX EST_MC) %then %do;
     %if %index(%upcase(&effect_type2), OR) or %index(%upcase(&effect_type2), RR) or %index(%upcase(&effect_type2), HR) %then %do;
         %let xaxis_value2=0.25 0.5 1 2 4;
      %end; 
      %else %do;
         %let xaxis_value2=-5 -2.5 0 2.5 5;
      %end;
   %end;
%end;
%else %do;
   %let xaxis_value1=%scan(&xaxis_value,1, |);
   %let xaxis_value2=%scan(&xaxis_value,2, |);
%end;

*=text_var;
%if &text_var EQ %then %do;
  %if %upcase(&fig_type) EQ REG %then %do;
     %let text_var1=label;
     %let text_var2=nopnt nepct estCI;
  %end;

  %if %upcase(&fig_type) EQ SUB %then %do;
     %let text_var1=label;
     %let text_var2=nopnt treat control estCI;
  %end;

  %if %upcase(&fig_type) EQ EST %then %do;
     %let text_var1=label;
     %let text_var2=treat control estCI;
  %end;

  %if %upcase(&fig_type) EQ META %then %do;
     %let text_var=label n_treat n_control nevent_treat nevent_control estCI ;
     %let text_var2=weight;
  %end;

  %if %upcase(&fig_type) EQ REG_MX %then %do;
     %let text_var1=label;
     %let text_var2=nopnt nepct estCI1 estCI2;
  %end;

   %if %upcase(&fig_type) EQ REG_MC %then %do;
     %let text_var1=label; 
     %let text_var2=nopnt;
     %let text_var3=nepct;
     %let text_var4=EstCI1;
     %let text_var5=EstCI2;
  %end;

  %if %upcase(&fig_type) EQ REG_MR %then %do;
     %let text_var1=label; 
     %let text_var2=EstCI;
  %end;


  %if %upcase(&fig_type) EQ SUB_MX %then %do;
     %let text_var1=label;
     %let text_var2=nopnt treat control estCI1 estCI2;
  %end;

  
   %if %upcase(&fig_type) EQ SUB_MC %then %do;
     %let text_var1=label; 
     %let text_var2=nopnt;
     %let text_var3=treat;
     %let text_var4=control;
     %let text_var5=EstCI1;
     %let text_var6=EstCI2;
  %end;

  %if %upcase(&fig_type) EQ SUB_MR %then %do;
     %let text_var1=label;
     %let text_var2=nopnt  treat control EstCI;
  %end;

  %if %upcase(&fig_type) EQ EST_MR %then %do;
     %let text_var1=label; 
     %let text_var2=EstCI;
  %end;

  %if %upcase(&fig_type) EQ EST_MX %then %do;
     %let text_var1=label;
     %let text_var2=treat control estCI1 estCI2;
  %end;

  %if %upcase(&fig_type) EQ EST_MC %then %do;
     %let text_var1=label; 
     %let text_var2=treat;
     %let text_var3=control;
     %let text_var4=EstCI1;
     %let text_var5=EstCI2;
  %end;
%end;



*=Plot var;
%if &plot_var EQ %then %do;
  %if %upcase(&fig_type) in  %str(REG_MR SUB_MR EST_MR) %then %do;
      %let obsid=grpid;
  %end;
  %else %do;
      %let obsid=obsid;
  %end;
   %let est1=est;
   %let lowerCL1=lowerCL;
   %let upperCL1=upperCL;
 %if %upcase(&fig_type) in %str(META REG_MX REG_MC SUB_MX SUB_MC EST_MX EST_MC) %then %do;
   %let est1=est1;
   %let lowerCL1=lowerCL1;
   %let upperCL1=upperCL1;

   %let est2=est2;
   %let lowerCL2=lowerCL2;
   %let upperCL2=upperCL2;
  %end;
%end;
%else %do;
   %let obsid=%scan(&plot_var, 1, |);
   %let est1=%scan(&plot_var, 2, |);
   %let lowerCL1=%scan(&plot_var, 3, |);
   %let upperCL1=%scan(&plot_var, 4, |);
   %let est2=%scan(&plot_var, 5, |);
   %let lowerCL2=%scan(&plot_var, 6, |);
   %let upperCL2=%scan(&plot_var, 6, |);
%end;



*===3. Plot forest plot;
options nodate nonumber  mprint missing=" ";
%if &fig_save NE %then %do;
	ods pdf file="&fig_save";
%end;


%if &fig_size NE %then %do;
    %let width=%qscan(&fig_size,1,*);
    %let height=%qscan(&fig_size,2,*);
	ods graphics /reset width=&width height=&height;
%end;
ods graphics/noborder;


*===3.1 REGRESSION FOREST PLOT;
%if %upcase(&fig_type) EQ REG %then %do;
	proc sgplot data=vardat noautolegend nowall noborder;
	  *==Plot Est and 95% CI;
	  highlow y=&obsID low=&lowerCL1 high=&upperCL1/type=line clipcap;
	  scatter y=&obsID x=&Est1/markerattrs=(symbol=circlefilled);
	  
	  *==Plot text;
	  yaxistable &text_var1/position=left labelattrs=(size=8) indentweight=indnt;
	  yaxistable &text_var2/position=left labelattrs=(size=8);
    
	  *==Axis setting;
	  xaxis label="&effect_type1 (95% CI)" values=(&xaxis_value1) valueattrs=(size=8) labelattrs=(size=8) type=&xaxis_type1;
	  yaxis type=discrete  reverse display=none colorbands=odd colorbandsattrs=(transparency=0) ;
	  refline  &refvalue1/axis=x;
	run;
%end;


%else %if %upcase(&fig_type) EQ REG_MR %then %do;
	proc sgplot data=vardat  nowall noborder;
	  *==Plot Est and 95% CI;
	  highlow y=&obsid low=&lowerCL1 high=&upperCL1/type=line  group=group groupdisplay=cluster clipcap;
	  scatter y=&obsid x=&Est1/markerattrs=(symbol=circlefilled) group=group groupdisplay=cluster ;

	  *==Plot Text;
	  yaxistable  &text_var1/position=left labelattrs=(size=8) indentweight=indnt;
	  yaxistable  &text_var2/position=left labelattrs=(size=8)  class=group classdisplay=cluster;

	  *==Settings;
	  xaxis label="&effect_type (95% CI)" values=(&xaxis_value1)  labelattrs=(size=8) type=&xaxis_type1;
	  yaxis type=discrete  reverse display=none  colorbands=odd colorbandsattrs=(transparency=0) ;
	  refline  &refvalue1 /axis=x;
	  *==Legend;
    %do i=1 %to &nrwlbl;
      legenditem type=markerline name="rw_&i" / label="&&rwlbl&i" lineattrs=GraphData&i markerattrs=GraphData&i; 
    %end;
    keylegend  %do i=1 %to &nrwlbl; "rw_&i"  %end; /location=outside position=top;      
	run;
%end;

%else %if %upcase(&fig_type) EQ REG_MX %then %do;
	proc sgplot data=vardat  nowall noborder;
	  *==Plot Est and 95% CI;
	  highlow y=&obsid low=&LowerCL2 high=&UpperCL2/type=line lineattrs=(thickness=0.2 color=red) x2axis clipcap;
	  highlow y=&obsid low=&LowerCL1 high=&UpperCL1/type=line lineattrs=(thickness=0.2 color=blue) clipcap;

	  scatter y=&obsid x=&Est2/markerattrs=(symbol=squarefilled color=red size=8) x2axis;
	  scatter y=&obsid x=&Est1/markerattrs=(symbol=squarefilled color=blue size=8);

	  *==Plot text;
	  yaxistable &text_var1/position=left labelattrs=(size=8)  indentweight=indnt ; 
    yaxistable &text_var2/position=left labelattrs=(size=8) ; 

	  *==Setting;
	  yaxis type=discrete  reverse display=none colorbands=odd colorbandsattrs=(transparency=0) ;
	  xaxis  label="&effect_type1 (95% CI)" values=(&xaxis_value1) valueattrs=(color=blue size=8) labelattrs=(color=blue size=8) type= &xaxis_type1;
	  x2axis label="&effect_type2 (95% CI)" values=(&xaxis_value2) valueattrs=(color=red size=8)  labelattrs=(color=red size=8) ;

	  refline  &refvalue1/axis=x lineattrs=(thickness=0.5 color=blue);
	  refline  &refvalue2/axis=x2 lineattrs=(thickness=0.5 color=red);
	run;
%end;



%else %if %upcase(&fig_type) EQ REG_MC %then %do;
proc template;
	define statgraph forest_reg_mc;
		begingraph;
			layout lattice / columns=7 columnweights=(0.15 0.1 0.1 0.15 0.15 0.15 0.15);
          /*==Column headers==*/
          sidebar / align=top;
            layout lattice /rows=2 columns=7 columnweights=(0.15 0.1 0.1 0.15 0.15 0.15 0.15);
              entry textattrs=(size=8) halign=left "Risk Factor";
              entry textattrs=(size=8) halign=left "No. of Patients";
              entry textattrs=(size=8) halign=left "No. (%) Event";
              entry textattrs=(size=8) halign=left "&effect_type1 (95% CI)";
              entry " ";
              entry textattrs=(size=8) halign=left "&effect_type2 (95% CI)";
              entry " ";
            endlayout;
          endsidebar;

		/*==First column for Effect label==*/
        layout overlay / walldisplay=none xaxisopts=(display=none)
                        yaxisopts=(type=discrete reverse=true 
                                   tickvalueattrs=(weight=bold) 
                                   discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)))  
                        yaxisopts=(display=none);

          axistable y=&obsid value=&text_var1/display=(values) textgroup=type  indentweight=indnt;
        endlayout;

        /*==Second column for No. of Patients==*/
        layout overlay / walldisplay=none xaxisopts=(display=none)
                         yaxisopts=(type=discrete reverse=true display=none
                                    tickvalueattrs=(weight=bold)
                                    discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
           axistable y=&obsid value=&text_var2/display=(values) textgroup=type;
        endlayout;

        /*==Third column for No. (%) Event==*/
          layout overlay / walldisplay=none xaxisopts=(display=none)
                           yaxisopts=(type=discrete reverse=true display=none
                           tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
            axistable y=&obsid value=&text_var3/display=(values) textgroup=type; 
          endlayout;

        /*==Fourth column for text of EstCI1==*/
          layout overlay / walldisplay=none xaxisopts=(display=none)
                           yaxisopts=(type=discrete reverse=true display=none
                           tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
          axistable y=&obsid value=&text_var4 /display=(values) textgroup=type; 
          endlayout;

        /*--Fifth column for plot of EstCI1*/
				layout overlay / yaxisopts=(type=discrete reverse=true display=none offsetmin=0 tickvalueattrs=(weight=bold) 
                                            discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6))) 
                                  walldisplay=none
					              xaxisopts=(label="&effect_type1 (95% CI)" tickvalueattrs=(size=8) labelattrs=(size=8) &axisopt1=(tickvaluelist=(&xaxis_value1)));
					highlowplot y=&obsid low=&LowerCL1 high=&UpperCL1/lineattrs=(thickness=0.2) clipcap=true;
					scatterplot y=&obsid x=&Est1/ markerattrs=(symbol=squarefilled);
					referenceline x= &refvalue1;
				endlayout;
                
                
        /*--Sixth column for text of EstCI2*/       
        layout overlay / walldisplay=none xaxisopts=(display=none)
                        yaxisopts=(type=discrete reverse=true display=none
                        tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
        axistable y=&obsid value=&text_var5 /display=(values) textgroup=type; 
        endlayout;

                
        /*--Seventh column for plot of EstCI2*/   
				layout overlay / yaxisopts=(type=discrete reverse=true display=none offsetmin=0 tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6))) 
                                walldisplay=none
					            xaxisopts=(type=log label="&effect_type2 (95% CI)" tickvalueattrs=(size=8) labelattrs=(size=8) &axisopt2=(tickvaluelist=(&xaxis_value2)));
					highlowplot y=&obsid low=&LowerCL2 high=&UpperCL2/ lineattrs=(thickness=0.2) clipcap=true;
					scatterplot y=&obsid x=&Est2/ markerattrs=(symbol=squarefilled);
					referenceline x= &refvalue2;
				endlayout;
                
			endlayout;
		endgraph;
	end;
run;

proc sgrender data=vardat template=forest_reg_mc;
run;
%end;


/*3.2 SUBGROUP FOREST PLOT*/
%else %if %upcase(&fig_type) EQ SUB %then %do;
	proc sgplot data=vardat  noautolegend nowall noborder nocycleattrs ;
	        *==Plot Est and 95% CI;
			highlow y=&obsid low=&lowerCL1 high=&upperCL1 /clipcap;
			scatter y=&obsid x=&est1/markerattrs=(symbol=squarefilled size=12);
    
			*==Plot text;
			yaxistable &text_var1/position=left labelattrs=(size=8) valueattrs=(size=8) indentweight=indnt;
			yaxistable &text_var2/position=left labelattrs=(size=8) valueattrs=(size=8);
			        
			*==Setting;
			xaxis label="&effect_type1 (95% CI)" values=(&xaxis_value1) labelattrs=(size=8) type=&xaxis_type1;
			yaxis type=discrete reverse display=none  colorbands=odd colorbandsattrs=(transparency=0);
	    refline  &refvalue1/axis=x;
	run; 
%end;


%else %if %upcase(&fig_type) EQ SUB_MR %then %do;
	proc sgplot data=vardat  nowall noborder;
	  *==Plot Est and 95% CI;
	  highlow y=&obsid low=&LowerCL1 high=&UpperCL1/type=line  group=group groupdisplay=cluster  clipcap;
	  scatter y=&obsid x=&Est1/markerattrs=(symbol=circlefilled) group=group groupdisplay=cluster ;

	  
	  *==Plot Text;
	  yaxistable &text_var1 /position=left labelattrs=(size=8)indentweight=indnt;
	  yaxistable &text_var2	/position=left labelattrs=(size=8) class=group classdisplay=cluster;

	  *==Settings;
	  xaxis label="&effect_type1 (95% CI)" values=(&xaxis_value1) labelattrs=(size=8)  type=&xaxis_type1;
	  yaxis type=discrete  reverse display=none  colorbands=odd colorbandsattrs=(transparency=0) ;
	  refline &refvalue1 /axis=x;

	  *==Legend;
	  %do i=1 %to &nrwlbl;
      legenditem type=markerline name="rw_&i" / label="&&rwlbl&i" lineattrs=GraphData&i markerattrs=GraphData&i; 
    %end;
    keylegend  %do i=1 %to &nrwlbl; "rw_&i"  %end; /location=outside position=top;     
	run;

%end;


%else %if %upcase(&fig_type) EQ SUB_MX %then %do;
	proc sgplot data=vardat  noautolegend nowall noborder nocycleattrs ;
	       *==Plot Est and 95% CI;
		  highlow y=&obsid low=&LowerCL2 high=&UpperCL2/type=line lineattrs=(thickness=0.2 color=red) x2axis clipcap;
		  highlow y=&obsid low=&LowerCL1 high=&UpperCL1/type=line lineattrs=(thickness=0.2 color=blue) clipcap;

		  scatter y=&obsid x=Est2/markerattrs=(symbol=squarefilled color=red size=8) x2axis;
		  scatter y=obsid x=Est1/markerattrs=(symbol=squarefilled color=blue size=8);
		
		 *==Plot text;
      yaxistable &text_var1/position=left labelattrs=(size=8)  indentweight=indnt; 
      yaxistable &text_var2/position=left labelattrs=(size=8); 
	        
		  *==Setting;
		  yaxis type=discrete  reverse display=none colorbands=odd colorbandsattrs=(transparency=0) ;
		  xaxis  label="&effect_type1 (95% CI)" values=(&xaxis_value1) valueattrs=(color=blue size=8) labelattrs=(color=blue size=8)  type=&xaxis_type1;
		  x2axis label="&effect_type2 (95% CI)" values=(&xaxis_value2) valueattrs=(color=red size=8)  labelattrs=(color=red size=8)   type=&xaxis_type2;

         refline  &refvalue1/axis=x lineattrs=(thickness=0.5 color=blue);
	     refline  &refvalue2/axis=x2 lineattrs=(thickness=0.5 color=red);
	run; 
%end;


%else %if %upcase(&fig_type) EQ SUB_MC %then %do;
proc template;
	define statgraph forest_sub_mc;
		begingraph;
			layout lattice / columns=8 columnweights=(0.1 0.1  0.1 0.1 0.15 0.15 0.15 0.15);
          /*==Column headers==*/
          sidebar / align=top;
            layout lattice /rows=2 columns=8 columnweights=(0.1 0.1 0.1 0.1 0.15 0.15 0.15 0.15);
              entry textattrs=(size=8) halign=left "Subgroup";
              entry textattrs=(size=8) halign=left "No. of Patients";
			  entry textattrs=(size=8) halign=left "Treat";
			  entry textattrs=(size=8) halign=left "Control";
              entry textattrs=(size=8) halign=left "&effect_type1 (95% CI)";
              entry " ";
              entry textattrs=(size=8) halign=left "&effect_type2 (95% CI)";
              entry " ";
            endlayout;
          endsidebar;

		/*==First column for Effect label==*/
        layout overlay / walldisplay=none xaxisopts=(display=none)
                        yaxisopts=(type=discrete reverse=true 
                                   tickvalueattrs=(weight=bold) 
                                   discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)))  
                        yaxisopts=(display=none);

          axistable y=&obsid value=&text_var1/display=(values) textgroup=type  indentweight=indnt;
        endlayout;

        /*==Second column for No. of Patients==*/
        layout overlay / walldisplay=none xaxisopts=(display=none)
                         yaxisopts=(type=discrete reverse=true display=none
                                    tickvalueattrs=(weight=bold)
                                    discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
           axistable y=&obsid value=&text_var2 /display=(values) textgroup=type;
        endlayout;

        /*==Third column for No. (%) Event in treat group==*/
          layout overlay / walldisplay=none xaxisopts=(display=none)
                           yaxisopts=(type=discrete reverse=true display=none
                           tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
            axistable y=&obsid value=&text_var3 /display=(values) textgroup=type; 
          endlayout;


		  /*==Forth column for No. (%) Event in treat group==*/
          layout overlay / walldisplay=none xaxisopts=(display=none)
                           yaxisopts=(type=discrete reverse=true display=none
                           tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
            axistable y=&obsid value=&text_var4 /display=(values) textgroup=type; 
          endlayout;



        /*==Fifth column for text of EstCI1==*/
          layout overlay / walldisplay=none xaxisopts=(display=none)
                           yaxisopts=(type=discrete reverse=true display=none
                           tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
           axistable y=&obsid value=&text_var5 /display=(values) textgroup=type; 
          endlayout;

          /*--Sixth column for plot of EstCI1*/
		     layout overlay / yaxisopts=(type=discrete reverse=true display=none offsetmin=0 tickvalueattrs=(weight=bold) 
                                    discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6))) 
                          walldisplay=none
			              xaxisopts=(type=&xaxis_type1 label="&effect_type1 (95% CI)" tickvalueattrs=(size=8) labelattrs=(size=8) &axisopt1=(tickvaluelist=(&xaxis_value1)));
            highlowplot y=&obsid low=&LowerCL1 high=&UpperCL1/lineattrs=(thickness=0.2) clipcap=true;
            scatterplot y=&obsid x=&Est1/ markerattrs=(symbol=squarefilled);
            referenceline x= &refvalue1;
		     endlayout;
                   
                
        /*--Seventh column for text of EstCI2*/       
        layout overlay / walldisplay=none xaxisopts=(display=none)
                        yaxisopts=(type=discrete reverse=true display=none
                        tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
        axistable y=&obsid value=&text_var6 /display=(values) textgroup=type; 
        endlayout;

                
        /*--Eighth column for plot of EstCI2*/   
				layout overlay / yaxisopts=(type=discrete reverse=true display=none offsetmin=0 tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6))) 
                                walldisplay=none
					            xaxisopts=(type=&xaxis_type2 label="&effect_type2 (95% CI)" tickvalueattrs=(size=8) labelattrs=(size=8) &axisopt2=(tickvaluelist=(&xaxis_value2)));
					highlowplot y=&obsid low=&LowerCL2 high=&UpperCL2/ lineattrs=(thickness=0.2) clipcap=true;
					scatterplot y=&obsid x=&Est2/ markerattrs=(symbol=squarefilled);
					referenceline x= &refvalue2;
				endlayout;
                
			endlayout;
		endgraph;
	end;
run;

proc sgrender data=vardat template=forest_sub_mc;
run;
%end;


/*3.3  EST FOREST PLOT*/
%else %if %upcase(&fig_type) EQ EST  %then %do;
	proc sgplot data=vardat  noautolegend nowall noborder nocycleattrs ;
	        *==Plot Est and 95% CI;
			highlow y=&obsid low=&lowerCL1 high=&upperCL1/clipcap;
			scatter y=&obsid x=&est1/markerattrs=(symbol=squarefilled size=8) ;
		
			*==Plot text;
			yaxistable &text_var1/position=left labelattrs=(size=8) valueattrs=(size=8) indentweight=indnt;
      yaxistable &text_var2/position=left labelattrs=(size=8) valueattrs=(size=8) ;

			*==Setting;
			xaxis label="&effect_type1 (95% CI)" values=(&xaxis_value1) labelattrs=(size=8)  type=&xaxis_type1;
			yaxis type=discrete reverse display=none  colorbands=odd colorbandsattrs=(transparency=0);
	    refline  &refvalue1 /axis=x;
	run; 
%end;

%else %if %upcase(&fig_type) EQ EST_MR %then %do;
	proc sgplot data=vardat  noautolegend nowall noborder nocycleattrs ;
	    *==Plot Est and 95% CI;
			highlow y=&obsid low=&lowerCL1 high=&upperCL1/group=group groupdisplay=cluster  clipcap;
			scatter y=&obsid x=&est1/markerattrs=(symbol=squarefilled size=8) group=group groupdisplay=cluster;
		
			*==Plot text;
			yaxistable &text_var1/position=left labelattrs=(size=8) valueattrs=(size=8) indentweight=indnt ;
      yaxistable &text_var2/position=left labelattrs=(size=8) valueattrs=(size=8)  class=group classdisplay=cluster;

			*==Setting;
			xaxis label="&effect_type1 (95% CI)" values=(&xaxis_value1) labelattrs=(size=8)  type=&xaxis_type1;
			yaxis type=discrete reverse display=none  colorbands=odd colorbandsattrs=(transparency=0);
	    refline  &refvalue1 /axis=x;

      *==Legend;
      %do i=1 %to &nrwlbl;
        legenditem type=markerline name="rw_&i" / label="&&rwlbl&i" lineattrs=GraphData&i markerattrs=GraphData&i; 
      %end;
      keylegend  %do i=1 %to &nrwlbl; "rw_&i"  %end; /location=outside position=top;    
	run; 
%end;


%else %if %upcase(&fig_type) EQ EST_MX %then %do;
	proc sgplot data=vardat  nowall noborder;
	  *==Plot Est and 95% CI;
	  highlow y=&obsid low=&LowerCL2 high=&UpperCL2/type=line lineattrs=(thickness=0.2 color=red) x2axis clipcap;
	  highlow y=&obsid low=&LowerCL1 high=&UpperCL1/type=line lineattrs=(thickness=0.2 color=blue) clipcap;

	  scatter y=&obsid x=&Est2/markerattrs=(symbol=squarefilled color=red size=8) x2axis;
	  scatter y=&obsid x=&Est1/markerattrs=(symbol=squarefilled color=blue size=8);

	  *==Plot text;
	  yaxistable &text_var1/position=left labelattrs=(size=7)  indentweight=indnt; 
    yaxistable &text_var2/position=left labelattrs=(size=7) ; 

	  *==Setting;
	  yaxis  type=discrete  reverse display=none colorbands=odd colorbandsattrs=(transparency=0) ;
	  xaxis  label="&effect_type1 (95% CI)" values=(&xaxis_value1) valueattrs=(color=blue size=8) labelattrs=(color=blue size=8)  type=&xaxis_type1;
	  x2axis  label="&effect_type2 (95% CI)" values=(&xaxis_value2) valueattrs=(color=red size=8)  labelattrs=(color=red size=8)  type=&xaxis_type2;
	  refline &refvalue1 /axis=x lineattrs=(thickness=0.5 color=blue);
	  refline &refvalue2/axis=x2 lineattrs=(thickness=0.5 color=red);
	run;	
%end;

%else %if %upcase(&fig_type) EQ EST_MC %then %do;
proc template;
	define statgraph forest_EST_mc;
		begingraph;
			layout lattice / columns=7 columnweights=(0.18 0.15 0.17 0.1 0.15 0.1 0.15);
          /*--Column headers--*/
          sidebar / align=top;
            layout lattice /rows=2 columns=7 columnweights=(0.18 0.15 0.17 0.1 0.15 0.1 0.15);
              entry textattrs=(size=8) halign=left "Outcomes";
              entry textattrs=(size=8) halign=left "Treat";
              entry textattrs=(size=8) halign=left "Control";
              entry textattrs=(size=8) halign=left "&effect_type1 (95% CI)";
              entry " ";
              entry textattrs=(size=8) halign=left "&effect_type2 (95% CI)";
              entry " ";
            endlayout;
          endsidebar;

		/*--First column for Outcome label*/
        layout overlay / walldisplay=none xaxisopts=(display=none)
                        yaxisopts=(type=discrete reverse=true 
                        tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)))  yaxisopts=(display=none);

          axistable y=&obsid value=&text_var1 /display=(values) textgroup=type  indentweight=indt;
        endlayout;

        /*--Second column for n(%)of event in treat group*/
        layout overlay / walldisplay=none xaxisopts=(display=none)
              yaxisopts=(type=discrete reverse=true display=none
              tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
           axistable y=&obsid value=&text_var2 /display=(values) textgroup=type;
        endlayout;

        /*--Third column for n(%) of event in control group*/
          layout overlay / walldisplay=none xaxisopts=(display=none)
                      yaxisopts=(type=discrete reverse=true display=none
                      tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
            axistable y=&obsid value=&text_var3 /display=(values) textgroup=type; 
          endlayout;

          /*--Fourth column for text of EstCI1*/
          layout overlay / walldisplay=none xaxisopts=(display=none)
                        yaxisopts=(type=discrete reverse=true display=none
                        tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
          axistable y=&obsid value=&text_var4 /display=(values) textgroup=type; 
          endlayout;

          /*--Fifth column for plot of EstCI1*/
				layout overlay / yaxisopts=(type=discrete reverse=true display=none offsetmin=0 tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6))) 
                                  walldisplay=none
			              xaxisopts=(type=&xaxis_type1 label="&effect_type1 (95% CI)" tickvalueattrs=(size=8) labelattrs=(size=8)   &axisopt1=(tickvaluelist=(&xaxis_value1)));
					highlowplot y=&obsid low=&LowerCL1 high=&UpperCL1/lineattrs=(thickness=0.2) clipcap=true;
					scatterplot y=&obsid x=&Est1/ markerattrs=(symbol=squarefilled);
					referenceline x=&refvalue1;
				endlayout;
                
                
        /*--Sixth column for text of EstCI2*/       
        layout overlay / walldisplay=none xaxisopts=(display=none)
                        yaxisopts=(type=discrete reverse=true display=none
                        tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6)));
        axistable y=&obsid value=&text_var5 /display=(values) textgroup=type; 
        endlayout;

                
        /*--Seventh column for plot of EstCI2*/   
				layout overlay / yaxisopts=(type=discrete reverse=true display=none offsetmin=0 tickvalueattrs=(weight=bold) discreteopts=(colorbands=odd  colorbandsattrs=(transparency=0.6))) 
                                walldisplay=none
			              xaxisopts=(type=&xaxis_type2 label="&effect_type2 (95% CI)" tickvalueattrs=(size=8) labelattrs=(size=8)   &axisopt2=(tickvaluelist=(&xaxis_value2)));
					highlowplot y=&obsid low=&LowerCL2 high=&UpperCL2/ lineattrs=(thickness=0.2) clipcap=true;
					scatterplot y=&obsid x=&Est2/ markerattrs=(symbol=squarefilled);
					referenceline x=&refvalue2;
				endlayout;            
			endlayout;
		endgraph;
	end;
run;

proc sgrender data=vardat template=forest_EST_mc;
run;

%end;


/*3.4 META FOREST PLOT*/
%else %if %upcase(&fig_type) EQ META %then %do;
proc sgplot data=vardat  noautolegend nowall noborder;
    
    *==high-low plot for EstCI;
    highlow y=&obsid low=&lowerCL1 high=&upperCL1/type=line clipcap; 

    *==plot of weight;
    bubble y=&obsid x=&Est1 size=weight;  
     
    *==plot of overall effect;
	  highlow y=&obsid low=&lowerCL2  high=&upperCL2/type=line; 
    scatter y=&obsid x=&est2 / markerattrs=(symbol=diamondFilled size=10);
    
    *==Plot of texts; 
    yaxistable  &text_var1/ position=left labelattrs=(size=8);
    yaxistable  &text_var2/ position=right labelattrs=(size=8);
  
	*==Setting;
    xaxis label="&effect_type1 (95% CI)" values=(&xaxis_value1) labelattrs=(size=8) valueattrs=(size=8) type=&xaxis_type1;
    yaxis display=none  reverse;  
	refline  &refvalue1 /axis=x;
run;
%end;


%else %do;
   %put  ERROR: The Value of FIGTYPE is Wrong!; 
   %abort; 
%end;

%if &fig_save NE %then %do;
   ods pdf close;
%end;
%mend ggForester;


