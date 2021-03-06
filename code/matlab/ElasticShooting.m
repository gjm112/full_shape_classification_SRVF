function q2n = ElasticShooting(q1,v)
%this is the distance from q to qnew 
d = sqrt(InnerProd_Q(v,v));
eps=.1;
if d < 0.00001
    %if the distance is small, stop and use this as the new q
    q2n = q1;
else
    %if it isn't small, use exp. map to project again until distance is
    %small enough to stop 
    q2{1} = cos(eps*d)*q1 + (sin(eps*d)/d)*v;
    q2{1} = ProjectC(q2{1});
    v = Parallel_Transport_C(v,q1,q2{1});
    d = sqrt(InnerProd_Q(v,v));
    for j=2:10
        q2{j} = cos(eps*d)*q2{j-1} + (sin(eps*d)/d)*v;
        q2{j} = ProjectC(q2{j});
        v = Parallel_Transport_C(v,q2{j-1},q2{j});
        d = sqrt(InnerProd_Q(v,v));
    end
    q2n = q2{10};
end