"""Rebuild publication figures from the supplied, frozen numerical outputs."""
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch

BASE=Path(__file__).resolve().parents[1]
DATA=BASE/'analysis'/'tables_csv'
FIG=BASE/'figures'
FIG.mkdir(parents=True, exist_ok=True)
ASSETS=['PAVE','KO','BTC-USD','PPLT','GLD']
U='QR + exact 2I+'
D='Selected 2I-FQR'
G='Student-t GARCH(1,1)'
CU='#65798A'; CD='#006F78'; CG='#BA713E'
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':10,'axes.titlesize':11,
 'axes.labelsize':10,'xtick.labelsize':9,'ytick.labelsize':9,'legend.fontsize':9,
 'axes.spines.top':False,'axes.spines.right':False,'axes.spines.left':False,
 'axes.spines.bottom':True,'axes.edgecolor':'#A5ACB3','axes.linewidth':.6,
 'savefig.facecolor':'white','pdf.fonttype':42,'ps.fonttype':42})

def read(name):return pd.read_csv(DATA/(name+'.csv'))
def grid(ax):
 ax.set_axisbelow(True);ax.grid(axis='y',color='#E1E5E8',linewidth=.6)
def save(fig,name):
 fig.savefig(FIG/name,dpi=600,bbox_inches='tight',pad_inches=.06)
 if name.endswith('.png'):
  fig.savefig(FIG/(Path(name).stem+'.pdf'),bbox_inches='tight',pad_inches=.06)
 plt.close(fig)
def facets(height=6.6):
 fig,axes=plt.subplots(3,2,figsize=(7.2,height))
 fig.subplots_adjust(left=.11,right=.99,bottom=.10,top=.96,hspace=.48,wspace=.32)
 axes=axes.ravel()
 for ax,asset in zip(axes,ASSETS):ax.set_title(asset,weight='bold',pad=7);grid(ax)
 axes[5].axis('off')
 return fig,axes

df=read('fixed_panel_adjusted_inference').set_index('Asset').loc[ASSETS]
fig,ax=plt.subplots(figsize=(6.9,3.5))
for i,(asset,row) in enumerate(df.iterrows()):
 val=row.TestMeanPinballGainPct
 ax.errorbar(val,i,xerr=[[val-row.PercentileCILowerPct],[row.PercentileCIUpperPct-val]],
  fmt='o',ms=6.5,mfc=CD if row.HolmAdjustedPValue<.05 else 'white',mec=CD,
  color=CD,elinewidth=1.3,capsize=3)
ax.axvline(0,color='#555B61',ls='--',lw=.9)
ax.set_yticks(range(5),ASSETS);ax.invert_yaxis();ax.set_ylim(4.55,-.55)
ax.set_xlabel('Mean pinball-loss gain (%)');grid(ax)
ax.legend(handles=[Line2D([],[],marker='o',ls='',color=CD,label='Holm p < 0.05'),
 Line2D([],[],marker='o',ls='',color=CD,mfc='white',label='Holm p ≥ 0.05')],
 loc='upper center',bbox_to_anchor=(.5,-.20),ncol=2,frameon=False)
fig.subplots_adjust(left=.13,right=.99,bottom=.27,top=.98)
save(fig,'fig01_primary_gain.pdf')

df=read('r1_horizon_loss_differences')
fig,axes=facets()
for ax,asset in zip(axes,ASSETS):
 d=df[df.Asset==asset].sort_values('Horizon')
 ax.fill_between(d.Horizon,d.Lower95*1e4,d.Upper95*1e4,color=CD,alpha=.16,lw=0)
 ax.plot(d.Horizon,d.MeanGain*1e4,color=CD,lw=1.6)
 ax.axhline(0,color='#6E7378',lw=.8,ls='--');ax.set_xlim(1,30);ax.set_xticks([1,10,20,30])
 for lab in ax.get_yticklabels():lab.set_fontsize(8.5)
axes[5].legend(handles=[Line2D([],[],color=CD,label='Unweighted − selected'),
 Patch(color=CD,alpha=.16,label='Pointwise 95% interval')],loc='center',frameon=False)
fig.supxlabel('Forecast horizon',y=.025,fontsize=11)
fig.supylabel('Mean pinball-loss difference (×10⁴)',x=.012,fontsize=11)
save(fig,'fig02_horizon_gains.pdf')

df=read('r1_distributional_benchmark_metrics')
fig,axes=facets()
for ax,asset in zip(axes,ASSETS):
 d=df[df.Asset==asset].set_index('Method').loc[[U,D,G]]
 ax.bar(range(3),d.MeanPinball*1e4,color=[CU,CD,CG],width=.64)
 ax.set_xticks(range(3),['U','D','G']);ax.set_ylim(bottom=0)
axes[5].legend(handles=[Patch(color=CU,label='U: Projected unweighted QR'),
 Patch(color=CD,label='D: Selected 2I-FQR'),Patch(color=CG,label='G: Student-t GARCH(1,1)')],
 loc='center',frameon=False,fontsize=9)
fig.supylabel('Mean test pinball loss (×10⁴)',x=.012,fontsize=11)
save(fig,'fig03_distributional_benchmark.pdf')

def empirical_coverage(name,field,target,filename):
 df=read(name)
 fig,ax=plt.subplots(figsize=(6.9,3.6));x=np.arange(5)
 for method,offset,color,label in [(U,-.12,CU,'Projected unweighted QR'),(D,.12,CD,'Selected 2I-FQR')]:
  d=df[df.Method==method].set_index('Asset').loc[ASSETS]
  ax.plot(x+offset,d[field]*100,'o',color=color,ms=6,label=label)
 ax.axhline(target*100,color='#555B61',ls='--',lw=.9,label=f'{target:.0%} target')
 ax.set_xticks(x,ASSETS);ax.set_ylim(87 if target==.90 else 94,100.7)
 ax.set_ylabel('Simultaneous path coverage (%)');grid(ax)
 ax.legend(loc='upper center',bbox_to_anchor=(.5,-.15),ncol=2,frameon=False)
 fig.subplots_adjust(left=.11,right=.99,bottom=.28,top=.98)
 save(fig,filename)

empirical_coverage('simultaneous_band_metrics','SimultaneousPathCoverage',.90,'fig04_empirical_path_coverage.png')

SCENARIOS=['Gaussian moderate persistence','Student t3.5 high persistence','Student t5 high persistence',
 'Student t5 short horizon','Student t5 variance regime shift','Student t5 with contamination']
def sim_coverage(prefix,target,filename):
 df=read('simulation_summary');fig,ax=plt.subplots(figsize=(6.9,3.65));x=np.arange(6)
 for method,offset,color,label in [(U,-.12,CU,'Projected unweighted QR'),('Depth QR + exact 2I+',.12,CD,'Depth-weighted QR')]:
  d=df[df.Method==method].set_index('Scenario').loc[SCENARIOS]
  m=d[prefix+'Mean'].to_numpy()*100
  se=d[prefix+'SD'].to_numpy()/np.sqrt(d.SuccessfulReplications.to_numpy())*100
  ax.errorbar(x+offset,m,yerr=1.96*se,fmt='o',color=color,ms=5.7,capsize=3,lw=1.1,label=label)
 ax.axhline(target*100,color='#555B61',ls='--',lw=.9,label=f'{target:.0%} target')
 ax.set_xticks(x,['S1','S2','S3','S4','S5','S6']);ax.set_ylim((76,94) if target==.90 else (84,97.5))
 ax.set_xlabel('Simulation design');ax.set_ylabel('Mean simultaneous path coverage (%)');grid(ax)
 ax.legend(loc='upper center',bbox_to_anchor=(.5,-.20),ncol=2,frameon=False)
 fig.subplots_adjust(left=.11,right=.99,bottom=.31,top=.98);save(fig,filename)

sim_coverage('SimultaneousPathCoverage',.90,'fig05_simulation_path_coverage.png')

df=read('r1_gamma_validation_profile')
fig,axes=facets()
for ax,asset in zip(axes,ASSETS):
 d=df[df.Asset==asset].sort_values('Gamma');y=(d.ValidationWIS-d.ValidationWIS.min())*1e4
 ax.plot(d.Gamma,y,color=CU,lw=1.2,zorder=1)
 for (_,r),v in zip(d.iterrows(),y):
  ax.plot(r.Gamma,v,marker='^' if r.SelectedGamma else 'o',ms=7 if r.SelectedGamma else 5,
   color=CD if r.SelectedGamma else CU,ls='',zorder=3)
 ax.set_xticks([0,.5,1,2],['0','0.5','1','2']);ax.set_ylim(bottom=-.04*max(y.max(),.1))
axes[5].legend(handles=[Line2D([],[],marker='^',ls='',color=CD,label='Selected exponent'),
 Line2D([],[],marker='o',ls='',color=CU,label='Other candidate')],loc='center',frameon=False)
fig.supxlabel('Depth exponent γ',y=.025,fontsize=11)
fig.supylabel('Validation WIS above minimum (×10⁴)',x=.012,fontsize=11)
save(fig,'fig06_gamma_profiles.pdf')

df=read('empirical_main_metrics')
methods=['QR raw','QR + quantile order',U,'Depth QR raw',D,'Training climatology','Curve persistence','Rolling historical quantiles']
short=['U-raw','U-q','U-2I','D-raw','D-2I','Clim','Pers','Roll']
fig,axes=facets(7.4)
for ax,asset in zip(axes,ASSETS):
 d=df[df.Asset==asset].set_index('Method').loc[methods]
 ax.bar(range(8),d.MeanPinball*1e4,color=[CU,CU,CU,CD,CD,'#989183','#B2A58F','#7C718E'],width=.72)
 ax.set_xticks(range(8),short,rotation=55,ha='right',fontsize=8);ax.set_ylim(bottom=0)
axes[5].text(.02,.98,'U: unweighted QR\nD: depth-weighted QR\nq: quantile order only\n2I: joint shape projection\nClim: training climatology\nPers: curve persistence\nRoll: rolling historical quantiles',
 va='top',fontsize=9.5,linespacing=1.65,transform=axes[5].transAxes)
fig.supylabel('Mean test pinball loss (×10⁴)',x=.012,fontsize=11)
save(fig,'fig07_regression_historical_scores.png')

COLORS=['#006F78','#B0642B','#416C9D','#81538B','#6D7A35']
df=read('quantile_calibration')
fig,ax=plt.subplots(figsize=(6.9,4.1))
for asset,color in zip(ASSETS,COLORS):
 d=df[df.Asset==asset].sort_values('Tau');ax.plot(d.Tau,d.ObservedCoverage,'o-',ms=4,lw=1.3,color=color,label=asset)
ax.plot([0,1],[0,1],ls='--',color='#7A8086',lw=.9)
ax.set(xlim=(0,1),ylim=(0,1),xlabel='Nominal quantile probability',ylabel='Pooled observed quantile coverage');grid(ax)
ax.legend(loc='upper center',bbox_to_anchor=(.5,-.17),ncol=3,frameon=False)
fig.subplots_adjust(left=.11,right=.99,bottom=.27,top=.98);save(fig,'fig08_quantile_reliability.png')

def horizon_coverage(name,field,target,filename):
 df=read(name);fig,ax=plt.subplots(figsize=(6.9,3.9))
 for asset,color in zip(ASSETS,COLORS):
  d=df[df.Asset==asset].sort_values('Horizon');ax.plot(d.Horizon,d[field]*100,color=color,lw=1.5,label=asset)
 ax.axhline(target*100,color='#555B61',ls='--',lw=.9)
 ax.set(xlim=(1,30),ylim=(target*100-1,100.6),xlabel='Forecast horizon',ylabel='Coordinatewise coverage (%)')
 ax.set_xticks([1,5,10,15,20,25,30]);grid(ax)
 ax.legend(loc='upper center',bbox_to_anchor=(.5,-.19),ncol=3,frameon=False)
 fig.subplots_adjust(left=.11,right=.99,bottom=.29,top=.98);save(fig,filename)

horizon_coverage('horizon_band_profiles','Coverage',.90,'fig09_band_coverage_horizon.png')
horizon_coverage('horizon_upper_band_profiles','UpperCoverage',.95,'fig10_upper_coverage_horizon.png')

df=read('horizon_band_profiles');fig,axes=facets(6.2)
for ax,asset in zip(axes,ASSETS):
 d=df[df.Asset==asset].sort_values('Horizon');ax.plot(d.Horizon,d.MeanWidth*1e3,color=CD,lw=1.6)
 ax.set_xlim(1,30);ax.set_xticks([1,10,20,30]);ax.set_ylim(bottom=0)
fig.supxlabel('Forecast horizon',y=.025,fontsize=11)
fig.supylabel('Mean two-sided band width (×10³)',x=.012,fontsize=11)
save(fig,'fig11_band_width_horizon.pdf')

empirical_coverage('simultaneous_upper_band_metrics','SimultaneousUpperPathCoverage',.95,'fig12_upper_path_coverage.png')
sim_coverage('UpperSimultaneousPathCoverage',.95,'fig13_simulation_upper_coverage.png')
print('Rebuilt 13 publication figures from frozen CSV outputs')
