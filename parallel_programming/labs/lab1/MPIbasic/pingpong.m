tab=[
% Put your results here (program output)
];
clf;
plot(tab(:,1),tab(:,2),'o-');
p=polyfit(tab(:,1)*8,tab(:,2),1);
hold on;
plot(tab(:,1),p(2)+p(1)*8*tab(:,1),'r-')
disp(['Latency : ' num2str(p(2)*1e6) 'us   Bandwidth : ' num2str(1/p(1)/1e6) ' MB/s'])


