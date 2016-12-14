package uk.co.turingatemyhamster.shortbol.j;

/**
 * Created by nmrp3 on 14/12/16.
 */
public class ShortbolTools {

    private static final ShortbolFacade facade = new ShortbolFacade();

    public static String convertShortbolToSbol(String shortbol) {
        return facade.convertShortbolToSbol(shortbol);
    }

}
