package kuru;

public class Kuru {
  public static abstract class Function<S,T> {
    public abstract T apply(S input);
  }

  public static class Constructor {
    int i;
    Object data;
  }

  public static class Tuple {
    public Object[] elements;

    public Tuple(int n) {
      elements = new Object[n];
    }
  }

  public static class Unit {
    private static Unit inst = new Unit();

    public static Unit instance() {
      return inst;
    }
  }

  public static class PolyEquals extends Function<Tuple,Boolean> {
    public Boolean apply(Tuple t) {
      Object operand1 = t.elements[0];
      Object operand2 = t.elements[1];
      return operand1.equals(operand2);
    }
  }

  public static Function<Tuple,Boolean> polyEquals = new PolyEquals();

  public static class Compiler {
    public static class Internal {
      public static Function<Tuple, Integer> intAdd = new Function<Tuple,Integer>() {
        public Integer apply(Tuple t) {
          Integer operand1 = (Integer) t.elements[0];
          Integer operand2 = (Integer) t.elements[1];
          return operand1 + operand2;
        }
      };

      public static Function<Tuple, Integer> intSub = new Function<Tuple,Integer>() {
        public Integer apply(Tuple t) {
          Integer operand1 = (Integer) t.elements[0];
          Integer operand2 = (Integer) t.elements[1];
          return operand1 - operand2;
        }
      };

      public static Function<Tuple, Integer> intMul = new Function<Tuple,Integer>() {
        public Integer apply(Tuple t) {
          Integer operand1 = (Integer) t.elements[0];
          Integer operand2 = (Integer) t.elements[1];
          return operand1 * operand2;
        }
      };

      public static Function<Tuple, Boolean> intLeq = new Function<Tuple,Boolean>() {
        public Boolean apply(Tuple t) {
          Integer operand1 = (Integer) t.elements[0];
          Integer operand2 = (Integer) t.elements[1];
          return operand1 <= operand2;
        }
      };

      public static Function<Object, Object> refNewref = new Function<Object, Object>() {
        public Object apply(Object t) {
          return null;
        }
      };

      public static Function<Tuple, Unit> refAssign = new Function<Tuple, Unit>() {
        public Unit apply(Tuple t) {
          return Unit.instance();
        }
      };

      public static Function<Tuple, Object> listCons = new Function<Tuple, Object>() {
        public Object apply(Tuple t) {
          return null;
        }
      };

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

