package kuru;

import java.util.List;
import java.util.LinkedList;

public class Kuru {
  public static abstract class Function<S,T> {
    public abstract T apply(S input);
  }

  public static class Constructor {
    public int i;
    public Object data;

    public Constructor() {
      i = 0;
      data = null;
    }

    public Constructor(int i, Object data) {
      this.i = i;
      this.data = data;
    }
  }

  public static class Tuple1<T0> {
    public T0 element0;
  }

  public static class Tuple2<T0,T1> {
    public T0 element0;
    public T1 element1;
  }

  public static class Tuple3<T0,T1,T2> {
    public T0 element0;
    public T1 element1;
    public T2 element2;
  }

  public static class Tuple4<T0,T1,T2,T3> {
    public T0 element0;
    public T1 element1;
    public T2 element2;
    public T3 element3;
  }

  public static class Tuple5<T0,T1,T2,T3,T4> {
    public T0 element0;
    public T1 element1;
    public T2 element2;
    public T3 element3;
    public T4 element4;
  }

  public static class Unit {
    private static Unit inst = new Unit();

    public static Unit instance() {
      return inst;
    }
  }

  public static class PolyEquals extends Function<Tuple2,Boolean> {
    public Boolean apply(Tuple2 t) {
      Object operand1 = t.element0;
      Object operand2 = t.element1;
      return operand1.equals(operand2);
    }
  }

  public static Function<Tuple2,Boolean> polyEquals = new PolyEquals();

  public static class Compiler {
    public static class Internal {
      public static Function<Tuple2<Integer,Integer>, Integer> intAdd = new Function<Tuple2<Integer,Integer>,Integer>() {
        public Integer apply(Tuple2<Integer,Integer> t) {
          return t.element0 + t.element1; 
        }
      };

      public static Function<Tuple2<Integer,Integer>, Integer> intSub = new Function<Tuple2<Integer,Integer>,Integer>() {
        public Integer apply(Tuple2<Integer,Integer> t) {
          return t.element0 - t.element1; 
        }
      };

      public static Function<Tuple2<Integer,Integer>, Integer> intMul = new Function<Tuple2<Integer,Integer>,Integer>() {
        public Integer apply(Tuple2<Integer,Integer> t) {
          return t.element0 * t.element1; 
        }
      };


      public static Function<Tuple2<Integer,Integer>, Boolean> intLeq = new Function<Tuple2<Integer,Integer>,Boolean>() {
        public Boolean apply(Tuple2<Integer,Integer> t) {
          return t.element0 <= t.element1; 
        }
      };

      public static Function<Object, Constructor> refNewref = new Function<Object, Constructor>() {
        public Constructor apply(Object t) {
          return null;
        }
      };

      public static Function<Tuple2<Constructor,Object>, Unit> refAssign = new Function<Tuple2<Constructor,Object>, Unit>() {
        public Unit apply(Tuple2<Constructor,Object> t) {
          return Unit.instance();
        }
      };

      public static class ListCons<S> extends Function<Tuple2<S,Constructor>,Constructor> {
        public Constructor apply(Tuple2<S,Constructor> t) {
          return new Constructor(1, t);
        }
      }

      public static ListCons listCons = new ListCons();

      public static Function<String, Integer> stringStrlen = new Function<String, Integer>() {
        public Integer apply(String t) {
          return t.length();
        }
      };

      public static Function<Integer, String> k_int_to_string = new Function<Integer, String>() {
        public String apply(Integer t) {
          return "" + t;
        }
      };

      public static Function<String, Unit> k_io_print = new Function<String, Unit>() {
        public Unit apply(String t) {
          System.out.print(t);
          return Unit.instance();
        }
      };
    }
  }
}

