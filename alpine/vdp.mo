
model Vdp
  type Height=Real(unit="m");
  type Velocity=Real(unit="m/s");
  parameter Real e=0.8;
  parameter Height h0=1.0;
  Height h(start = h0);
  Velocity v;
  input Real cop(start = 3.0);
equation
  v = der(h);
  der(v) = -9.81;
  when h<0 then
    reinit(v, -e*pre(v));
  end when;
end Vdp;

