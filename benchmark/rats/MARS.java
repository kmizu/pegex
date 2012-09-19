/* $Id: MARS.java,v 1.1 2004/01/19 02:03:50 rgrimm Exp $
 *
 * Copyright (C) 1997-2000 The Cryptix Foundation Limited.
 * All rights reserved.
 *
 * Use, modification, copying and distribution of this software is subject
 * the terms and conditions of the Cryptix General Licence. You should have
 * received a copy of the Cryptix General Licence along with this library;
 * if not, you can download a copy from http://www.cryptix.org/ .
 */
package cryptix.jce.provider.cipher;


import java.security.InvalidKeyException;
import java.security.Key;


/**
 * MARS is an AES candidate submitted by IBM.
 * <p>
 * MARS was designed by Carolynn Burwick, Don Coppersmith, Edward D'Avignon,
 * Rosario Gennaro, Shai Halevi, Charanjit Jutla, Stephen M. Matyas Jr.,
 * Luke O'Connor, Mohammad Peyravian, David Safford, and Nevenko Zunic.
 * <p>
 * Please note that this is the 'amended' 2nd round version.
 *
 * @version $Revision: 1.1 $
 * @author  Jeroen C. van Gelderen (gelderen@cryptix.org)
 * @author  Raif S. Naffah
 */
public final class MARS extends BlockCipher
{

// Constants
//...........................................................................

    static final int
        BLOCK_SIZE = 16, // bytes in a data-block
        ROUNDS     = 32; // rounds of a MARS cipher


    /** MARS S-box */
    private static final int[] S = {
        0x09D0C479, 0x28C8FFE0, 0x84AA6C39, 0x9DAD7287,
        0x7DFF9BE3, 0xD4268361, 0xC96DA1D4, 0x7974CC93,
        0x85D0582E, 0x2A4B5705, 0x1CA16A62, 0xC3BD279D,
        0x0F1F25E5, 0x5160372F, 0xC695C1FB, 0x4D7FF1E4,
        0xAE5F6BF4, 0x0D72EE46, 0xFF23DE8A, 0xB1CF8E83,
        0xF14902E2, 0x3E981E42, 0x8BF53EB6, 0x7F4BF8AC,
        0x83631F83, 0x25970205, 0x76AFE784, 0x3A7931D4,
        0x4F846450, 0x5C64C3F6, 0x210A5F18, 0xC6986A26,
        0x28F4E826, 0x3A60A81C, 0xD340A664, 0x7EA820C4,
        0x526687C5, 0x7EDDD12B, 0x32A11D1D, 0x9C9EF086,
        0x80F6E831, 0xAB6F04AD, 0x56FB9B53, 0x8B2E095C,
        0xB68556AE, 0xD2250B0D, 0x294A7721, 0xE21FB253,
        0xAE136749, 0xE82AAE86, 0x93365104, 0x99404A66,
        0x78A784DC, 0xB69BA84B, 0x04046793, 0x23DB5C1E,
        0x46CAE1D6, 0x2FE28134, 0x5A223942, 0x1863CD5B,
        0xC190C6E3, 0x07DFB846, 0x6EB88816, 0x2D0DCC4A,
        0xA4CCAE59, 0x3798670D, 0xCBFA9493, 0x4F481D45,
        0xEAFC8CA8, 0xDB1129D6, 0xB0449E20, 0x0F5407FB,
        0x6167D9A8, 0xD1F45763, 0x4DAA96C3, 0x3BEC5958,
        0xABABA014, 0xB6CCD201, 0x38D6279F, 0x02682215,
        0x8F376CD5, 0x092C237E, 0xBFC56593, 0x32889D2C,
        0x854B3E95, 0x05BB9B43, 0x7DCD5DCD, 0xA02E926C,
        0xFAE527E5, 0x36A1C330, 0x3412E1AE, 0xF257F462,
        0x3C4F1D71, 0x30A2E809, 0x68E5F551, 0x9C61BA44,
        0x5DED0AB8, 0x75CE09C8, 0x9654F93E, 0x698C0CCA,
        0x243CB3E4, 0x2B062B97, 0x0F3B8D9E, 0x00E050DF,
        0xFC5D6166, 0xE35F9288, 0xC079550D, 0x0591AEE8,
        0x8E531E74, 0x75FE3578, 0x2F6D829A, 0xF60B21AE,
        0x95E8EB8D, 0x6699486B, 0x901D7D9B, 0xFD6D6E31,
        0x1090ACEF, 0xE0670DD8, 0xDAB2E692, 0xCD6D4365,
        0xE5393514, 0x3AF345F0, 0x6241FC4D, 0x460DA3A3,
        0x7BCF3729, 0x8BF1D1E0, 0x14AAC070, 0x1587ED55,
        0x3AFD7D3E, 0xD2F29E01, 0x29A9D1F6, 0xEFB10C53,
        0xCF3B870F, 0xB414935C, 0x664465ED, 0x024ACAC7,
        0x59A744C1, 0x1D2936A7, 0xDC580AA6, 0xCF574CA8,
        0x040A7A10, 0x6CD81807, 0x8A98BE4C, 0xACCEA063,
        0xC33E92B5, 0xD1E0E03D, 0xB322517E, 0x2092BD13,
        0x386B2C4A, 0x52E8DD58, 0x58656DFB, 0x50820371,
        0x41811896, 0xE337EF7E, 0xD39FB119, 0xC97F0DF6,
        0x68FEA01B, 0xA150A6E5, 0x55258962, 0xEB6FF41B,
        0xD7C9CD7A, 0xA619CD9E, 0xBCF09576, 0x2672C073,
        0xF003FB3C, 0x4AB7A50B, 0x1484126A, 0x487BA9B1,
        0xA64FC9C6, 0xF6957D49, 0x38B06A75, 0xDD805FCD,
        0x63D094CF, 0xF51C999E, 0x1AA4D343, 0xB8495294,
        0xCE9F8E99, 0xBFFCD770, 0xC7C275CC, 0x378453A7,
        0x7B21BE33, 0x397F41BD, 0x4E94D131, 0x92CC1F98,
        0x5915EA51, 0x99F861B7, 0xC9980A88, 0x1D74FD5F,
        0xB0A495F8, 0x614DEED0, 0xB5778EEA, 0x5941792D,
        0xFA90C1F8, 0x33F824B4, 0xC4965372, 0x3FF6D550,
        0x4CA5FEC0, 0x8630E964, 0x5B3FBBD6, 0x7DA26A48,
        0xB203231A, 0x04297514, 0x2D639306, 0x2EB13149,
        0x16A45272, 0x532459A0, 0x8E5F4872, 0xF966C7D9,
        0x07128DC0, 0x0D44DB62, 0xAFC8D52D, 0x06316131,
        0xD838E7CE, 0x1BC41D00, 0x3A2E8C0F, 0xEA83837E,
        0xB984737D, 0x13BA4891, 0xC4F8B949, 0xA6D6ACB3,
        0xA215CDCE, 0x8359838B, 0x6BD1AA31, 0xF579DD52,
        0x21B93F93, 0xF5176781, 0x187DFDDE, 0xE94AEB76,
        0x2B38FD54, 0x431DE1DA, 0xAB394825, 0x9AD3048F,
        0xDFEA32AA, 0x659473E3, 0x623F7863, 0xF3346C59,
        0xAB3AB685, 0x3346A90B, 0x6B56443E, 0xC6DE01F8,
        0x8D421FC0, 0x9B0ED10C, 0x88F1A1E9, 0x54C1F029,
        0x7DEAD57B, 0x8D7BA426, 0x4CF5178A, 0x551A7CCA,
        0x1A9A5F08, 0xFCD651B9, 0x25605182, 0xE11FC6C3,
        0xB6FD9676, 0x337B3027, 0xB7C8EB14, 0x9E5FD030,
        0x6B57E354, 0xAD913CF7, 0x7E16688D, 0x58872A69,
        0x2C2FC7DF, 0xE389CCC6, 0x30738DF1, 0x0824A734,
        0xE1797A8B, 0xA4A8D57B, 0x5B5D193B, 0xC8A8309B,
        0x73F9A978, 0x73398D32, 0x0F59573E, 0xE9DF2B03,
        0xE8A5B6C8, 0x848D0704, 0x98DF93C2, 0x720A1DC3,
        0x684F259A, 0x943BA848, 0xA6370152, 0x863B5EA3,
        0xD17B978B, 0x6D9B58EF, 0x0A700DD4, 0xA73D36BF,
        0x8E6A0829, 0x8695BC14, 0xE35B3447, 0x933AC568,
        0x8894B022, 0x2F511C27, 0xDDFBCC3C, 0x006662B6,
        0x117C83FE, 0x4E12B414, 0xC2BCA766, 0x3A2FEC10,
        0xF4562420, 0x55792E2A, 0x46F5D857, 0xCEDA25CE,
        0xC3601D3B, 0x6C00AB46, 0xEFAC9C28, 0xB3C35047,
        0x611DFEE3, 0x257C3207, 0xFDD58482, 0x3B14D84F,
        0x23BECB64, 0xA075F3A3, 0x088F8EAD, 0x07ADF158,
        0x7796943C, 0xFACABF3D, 0xC09730CD, 0xF7679969,
        0xDA44E9ED, 0x2C854C12, 0x35935FA3, 0x2F057D9F,
        0x690624F8, 0x1CB0BAFD, 0x7B0DBDC6, 0x810F23BB,
        0xFA929A1A, 0x6D969A17, 0x6742979B, 0x74AC7D05,
        0x010E65C4, 0x86A3D963, 0xF907B5A0, 0xD0042BD3,
        0x158D7D03, 0x287A8255, 0xBBA8366F, 0x096EDC33,
        0x21916A7B, 0x77B56B86, 0x951622F9, 0xA6C5E650,
        0x8CEA17D1, 0xCD8C62BC, 0xA3D63433, 0x358A68FD,
        0x0F9B9D3C, 0xD6AA295B, 0xFE33384A, 0xC000738E,
        0xCD67EB2F, 0xE2EB6DC2, 0x97338B02, 0x06C9F246,
        0x419CF1AD, 0x2B83C045, 0x3723F18A, 0xCB5B3089,
        0x160BEAD7, 0x5D494656, 0x35F8A74B, 0x1E4E6C9E,
        0x000399BD, 0x67466880, 0xB4174831, 0xACF423B2,
        0xCA815AB3, 0x5A6395E7, 0x302A67C5, 0x8BDB446B,
        0x108F8FA4, 0x10223EDA, 0x92B8B48B, 0x7F38D0EE,
        0xAB2701D4, 0x0262D415, 0xAF224A30, 0xB3D88ABA,
        0xF8B2C3AF, 0xDAF7EF70, 0xCC97D3B7, 0xE9614B6C,
        0x2BAEBFF4, 0x70F687CF, 0x386C9156, 0xCE092EE5,
        0x01E87DA6, 0x6CE91E6A, 0xBB7BCC84, 0xC7922C20,
        0x9D3B71FD, 0x060E41C6, 0xD7590F15, 0x4E03BB47,
        0x183C198E, 0x63EEB240, 0x2DDBF49A, 0x6D5CBA54,
        0x923750AF, 0xF9E14236, 0x7838162B, 0x59726C72,
        0x81B66760, 0xBB2926C1, 0x48A0CE0D, 0xA6C0496D,
        0xAD43507B, 0x718D496A, 0x9DF057AF, 0x44B1BDE6,
        0x054356DC, 0xDE7CED35, 0xD51A138B, 0x62088CC9,
        0x35830311, 0xC96EFCA2, 0x686F86EC, 0x8E77CB68,
        0x63E1D6B8, 0xC80F9778, 0x79C491FD, 0x1B4C67F2,
        0x72698D7D, 0x5E368C31, 0xF7D95E2E, 0xA1D3493F,
        0xDCD9433E, 0x896F1552, 0x4BC4CA7A, 0xA6D1BAF4,
        0xA5A96DCC, 0x0BEF8B46, 0xA169FDA7, 0x74DF40B7,
        0x4E208804, 0x9A756607, 0x038E87C8, 0x20211E44,
        0x8B7AD4BF, 0xC6403F35, 0x1848E36D, 0x80BDB038,
        0x1E62891C, 0x643D2107, 0xBF04D6F8, 0x21092C8C,
        0xF644F389, 0x0778404E, 0x7B78ADB8, 0xA2C52D53,
        0x42157ABE, 0xA2253E2E, 0x7BF3F4AE, 0x80F594F9,
        0x953194E7, 0x77EB92ED, 0xB3816930, 0xDA8D9336,
        0xBF447469, 0xF26D9483, 0xEE6FAED5, 0x71371235,
        0xDE425F73, 0xB4E59F43, 0x7DBE2D4E, 0x2D37B185,
        0x49DC9A63, 0x98C39D98, 0x1301C9A2, 0x389B1BBF,
        0x0C18588D, 0xA421C1BA, 0x7AA3865C, 0x71E08558,
        0x3C5CFCAA, 0x7D239CA4, 0x0297D9DD, 0xD7DC2830,
        0x4B37802B, 0x7428AB54, 0xAEEE0347, 0x4B3FBB85,
        0x692F2F08, 0x134E578E, 0x36D9E0BF, 0xAE8B5FCF,
        0xEDB93ECF, 0x2B27248E, 0x170EB1EF, 0x7DC57FD6,
        0x1E760F16, 0xB1136601, 0x864E1B9B, 0xD7EA7319,
        0x3AB871BD, 0xCFA4D76F, 0xE31BD782, 0x0DBEB469,
        0xABB96061, 0x5370F85D, 0xFFB07E37, 0xDA30D0FB,
        0xEBC977B6, 0x0B98B40F, 0x3A4D0FE6, 0xDF4FC26B,
        0x159CF22A, 0xC298D6E2, 0x2B78EF6A, 0x61A94AC0,
        0xAB561187, 0x14EEA0F0, 0xDF0D4164, 0x19AF70EE };


// Instance variables
//...........................................................................

    /** Encrypt (false) or decrypt mode (true) */
    private boolean decrypt;

    /** Subkeys (40). */
    private final int[] K = new int[40];


// Constructor
//...........................................................................

    public MARS()
    {
        super(BLOCK_SIZE);
    }


// BlockCipher abstract method implementation
//...........................................................................

    protected void coreInit(Key key, boolean decrypt)
    throws InvalidKeyException
    {
        if( key==null )
            throw new InvalidKeyException("key: key is null");

        if( !key.getFormat().equalsIgnoreCase("RAW") )
            throw new InvalidKeyException("key: wrong format, RAW needed");

        byte[] userkey = key.getEncoded();
        if(userkey == null)
            throw new InvalidKeyException("RAW bytes missing");

        int len = userkey.length ;
        if( len != 16 && len != 24 && len!=32 )
            throw new InvalidKeyException("Invalid user key length");

        generateSubKeys(userkey);
        this.decrypt   = decrypt;
    }


    protected void coreCrypt(byte[] in, int inOffset, byte[] out, int outOffset)
    {
        if( decrypt )
            blockDecrypt(in, inOffset, out, outOffset);
        else
            blockEncrypt(in, inOffset, out, outOffset);
    }


// Helper methods
//...........................................................................

   /** Expand a session key into 40 MARS subkeys in int[] this.K . */
    private final void generateSubKeys(byte[] key)
    {
        int keyLen = key.length;
        int n      = keyLen / 4;

        int[] K = this.K;
        int[] T = new int[15];                                        // (3)
        int[] B = { 0xa4a8d57b, 0x5b5d193b, 0xc8a8309b, 0x73f9a978 }; // (4,5,6)

        int i;
        for(i = 0; i < keyLen; i++)
            T[i/4] |= (key[i] & 0xFF) << (i*8);

        T[i/4] = i/4;

        int j, ii;
        for(j=0; j<4; j++)
        {
            // Do linear transformation
            for(i=0; i<15; i++)
                T[i] ^= rotl(T[(i+8) % 15] ^ T[(i+13) % 15], 3) ^ (4*i+j);

            // Do four rounds of stirring
            for(ii=0; ii<4; ii++)
                for(i=0; i<15; i++)
                    T[i] = rotl(T[i] + S[ T[(i+14) % 15] & 0x1FF], 9);

            // Store next 10 key words into K[]
            for(i=0; i<10; i++)
                K[10*j+i] = T[(4*i) % 15];
        }

        // Modify multiplication key-words
        int m, p, r, w;
        for(i=5; i<=35; i+=2)
        {
            j = K[i] & 0x3;
            w = K[i] | 0x3;

            m = maskFrom(w);
            r = K[i-1] & 0x1F;
            p = rotl(B[j], r);

            K[i] = w ^ (p & m);
        }
    }


    /**
     * Generate a bit-mask M from x.
     *
     * Bit M{i}=1 iff x{i} belongs to a sequence of 10 consecutive 0's
     * or 1's in x, and also 2 <= i <= 30 and x{i-1} = w{i} = w{i+1}.
     *
     * Code taken from the MARS implementation in C/C++ by
     * Dr. B. R. Gladman (brian.gladman@btinternet.com).
     *
     * This is used during key expansion.
     */
    private static int maskFrom(int x)
    {
        int m;

        // Set m{bn} = 1 if x{bn} == x{bn+1} for 0 <= bn <= 30.
        // That is, set a bit in m if the corresponding bit and the
        // next higher bit in x are equal in value (set m{31} = 0).
        m = (~x ^ (x >>> 1)) & 0x7fffffff;

        // Sequences of 9 '1' bits in m now correspond to sequences
        // of 10 '0's or 10 '1' bits in x.  Shift and 'and' bits in
        // m to find sequences of 9 or more '1' bits.   As a result
        // bits in m are set if they are at the bottom of sequences
        // of 10 adjacent '0's or 10 adjacent '1's in x.
        m &= (m >>> 1) & (m >>> 2);
        m &= (m >>> 3) & (m >>> 6);

        // We need the internal bits in each continuous sequence of
        // matching bits (that is the bits less the two endpoints).
        // We thus propagate each set bit into the 8 internal bits
        // that it represents, starting 1 left and finsihing 8 left
        // of its position.
        m <<= 1;
        m |= (m << 1);
        m |= (m << 2);
        m |= (m << 4);

        return m & 0xfffffffc;
    }


    /** Rotate left an int by the specified amount. */
    private static int rotl(int arg, int amount)
    {
        return (arg << amount) | (arg >>> (32-amount));
    }


    /** Encrypt exactly one block of plaintext. */
    private final void blockEncrypt(byte[] in, int inOffset,
                                    byte[] out, int outOffset)
    {
        int D0 = (in[inOffset++] & 0xFF)       |
                 (in[inOffset++] & 0xFF) <<  8 |
                 (in[inOffset++] & 0xFF) << 16 |
                 (in[inOffset++] & 0xFF) << 24;
        int D1 = (in[inOffset++] & 0xFF)       |
                 (in[inOffset++] & 0xFF) <<  8 |
                 (in[inOffset++] & 0xFF) << 16 |
                 (in[inOffset++] & 0xFF) << 24;
        int D2 = (in[inOffset++] & 0xFF)       |
                 (in[inOffset++] & 0xFF) <<  8 |
                 (in[inOffset++] & 0xFF) << 16 |
                 (in[inOffset++] & 0xFF) << 24;
        int D3 = (in[inOffset++] & 0xFF)       |
                 (in[inOffset++] & 0xFF) <<  8 |
                 (in[inOffset++] & 0xFF) << 16 |
                 (in[inOffset  ] & 0xFF) << 24;

        // 1. key addition                                 // (1)
        D0 += K[0];
        D1 += K[1];
        D2 += K[2];
        D3 += K[3];

        // 2. forward mixing
        int i, t;
        for (i = 0; i < 8; i++)                             // (5)
        {
            D1 ^= S[        D0         & 0xFF ];            // (7)
            D1 += S[256 + ((D0 >>>  8) & 0xFF)];            // (8)
            D2 += S[       (D0 >>> 16) & 0xFF ];            // (9)
            D3 ^= S[256 + ((D0 >>> 24) & 0xFF)];            // (10)
            D0 = D0 >>> 24 | D0 << 8;                       // (12)

            switch (i)
            {
            case 0:
            case 4: D0 += D3; break;                        // (15)
            case 1:
            case 5: D0 += D1; break;                        // (17)
            }

            t = D0; D0 = D1; D1 = D2; D2 = D3; D3 = t;      // (19)
        }                                                   // (20)

        // 3. Keyed transformation
        int[] ia;
        for (i = 0; i < 16; i++)                            // (22)
        {
            ia = E(D0, K[2*i + 4], K[2*i + 5]);             // (23)
            D0 = D0 << 13 | D0 >>> 19;                      // (24)
            D2 += ia[1];                                    // (25)
            if (i < 8)
            {
                D1 += ia[0];                                // (27)
                D3 ^= ia[2];                                // (28)
            }
            else
            {
                D3 += ia[0];                                // (30)
                D1 ^= ia[2];                                // (31)
            }
            t = D0; D0 = D1; D1 = D2; D2 = D3; D3 = t;      // (34)
        }                                                   // (35)

        // 4. Backward mixing
        for (i = 0; i < 8; i++)                             // (37)
        {
            switch (i)
            {
            case 2:
            case 6: D0 -= D3; break;                        // (40)
            case 3:
            case 7: D0 -= D1; break;                        // (42)
            }
            D1 ^= S[256 +  (D0         & 0xFF)];            // (44)
            D2 -= S[       (D0 >>> 24) & 0xFF ];            // (45)
            D3 -= S[256 + ((D0 >>> 16) & 0xFF)];            // (46)
            D3 ^= S[       (D0 >>>  8) & 0xFF ];            // (47)
            D0 = D0 << 24 | D0 >>> 8;                       // (49)
            t = D0; D0 = D1; D1 = D2; D2 = D3; D3 = t;      // (51)
        }                                                   // (52)

        D0 -= K[36];
        D1 -= K[37];
        D2 -= K[38];
        D3 -= K[39];

        out[outOffset++] = (byte)(D0       );
        out[outOffset++] = (byte)(D0 >>>  8);
        out[outOffset++] = (byte)(D0 >>> 16);
        out[outOffset++] = (byte)(D0 >>> 24);

        out[outOffset++] = (byte)(D1       );
        out[outOffset++] = (byte)(D1 >>>  8);
        out[outOffset++] = (byte)(D1 >>> 16);
        out[outOffset++] = (byte)(D1 >>> 24);

        out[outOffset++] = (byte)(D2       );
        out[outOffset++] = (byte)(D2 >>>  8);
        out[outOffset++] = (byte)(D2 >>> 16);
        out[outOffset++] = (byte)(D2 >>> 24);

        out[outOffset++] = (byte)(D3       );
        out[outOffset++] = (byte)(D3 >>>  8);
        out[outOffset++] = (byte)(D3 >>> 16);
        out[outOffset  ] = (byte)(D3 >>> 24);
    }


    /** Decrypt exactly one block of ciphertext. */
    private final void blockDecrypt(byte[] in, int inOffset,
                                    byte[] out, int outOffset)
    {
        int D0 = (in[inOffset++] & 0xFF)       |
                 (in[inOffset++] & 0xFF) <<  8 |
                 (in[inOffset++] & 0xFF) << 16 |
                 (in[inOffset++] & 0xFF) << 24;
        int D1 = (in[inOffset++] & 0xFF)       |
                 (in[inOffset++] & 0xFF) <<  8 |
                 (in[inOffset++] & 0xFF) << 16 |
                 (in[inOffset++] & 0xFF) << 24;
        int D2 = (in[inOffset++] & 0xFF)       |
                 (in[inOffset++] & 0xFF) <<  8 |
                 (in[inOffset++] & 0xFF) << 16 |
                 (in[inOffset++] & 0xFF) << 24;
        int D3 = (in[inOffset++] & 0xFF)       |
                 (in[inOffset++] & 0xFF) <<  8 |
                 (in[inOffset++] & 0xFF) << 16 |
                 (in[inOffset  ] & 0xFF) << 24;

        // 1. key addition
        D0 += K[36];
        D1 += K[37];
        D2 += K[38];
        D3 += K[39];

        // 2. forward mixing                                // (1)
        int i, t;
        for (i = 7; i >= 0; i--)                            // (5)
        {
            t = D3; D3 = D2; D2 = D1; D1 = D0; D0 = t;      // (7)
            D0 = D0 >>> 24 | D0 << 8;                       // (9)
            D3 ^= S[       (D0 >>>  8) & 0xFF ];            // (11)
            D3 += S[256 + ((D0 >>> 16) & 0xFF)];            // (12)
            D2 += S[       (D0 >>> 24) & 0xFF ];            // (13)
            D1 ^= S[256 +  (D0         & 0xFF)];            // (14)
            switch (i)
            {
            case 2:
            case 6: D0 += D3; break;                        // (17)
            case 3:
            case 7: D0 += D1; break;                        // (19)
            }
        }                                                   // (20)

        // 3. Keyed transformation
        int[] ia;
        for (i = 15; i >= 0; i--)                           // (22)
        {
            t = D3; D3 = D2; D2 = D1; D1 = D0; D0 = t;      // (24)
            D0 = D0 >>> 13 | D0 << 19;                      // (25)
            ia = E(D0, K[2*i + 4], K[2*i + 5]);             // (26)
            D2 -= ia[1];                                    // (27)
            if (i < 8)
            {
                D1 -= ia[0];                                // (29)
                D3 ^= ia[2];                                // (30)
            }
            else
            {
                D3 -= ia[0];                                // (32)
                D1 ^= ia[2];                                // (33)
            }
        }                                                   // (35)

        // 4. Backward mixing
        for (i = 7; i >= 0; i--)                            // (37)
        {
            t = D3; D3 = D2; D2 = D1; D1 = D0; D0 = t;      // (39)
            switch (i)
            {
            case 0:
            case 4: D0 -= D3; break;                        // (42)
            case 1:
            case 5: D0 -= D1; break;                        // (44)
            }
            D0 = D0 << 24 | D0 >>> 8;                       // (46)
            D3 ^= S[256 + ((D0 >>> 24) & 0xFF)];            // (48)
            D2 -= S[       (D0 >>> 16) & 0xFF ];            // (49)
            D1 -= S[256 + ((D0 >>>  8) & 0xFF)];            // (50)
            D1 ^= S[        D0         & 0xFF ];            // (51)
        }                                                   // (52)

        D0 -= K[0];
        D1 -= K[1];
        D2 -= K[2];
        D3 -= K[3];

        out[outOffset++] = (byte)(D0       );
        out[outOffset++] = (byte)(D0 >>>  8);
        out[outOffset++] = (byte)(D0 >>> 16);
        out[outOffset++] = (byte)(D0 >>> 24);

        out[outOffset++] = (byte)(D1       );
        out[outOffset++] = (byte)(D1 >>>  8);
        out[outOffset++] = (byte)(D1 >>> 16);
        out[outOffset++] = (byte)(D1 >>> 24);

        out[outOffset++] = (byte)(D2       );
        out[outOffset++] = (byte)(D2 >>>  8);
        out[outOffset++] = (byte)(D2 >>> 16);
        out[outOffset++] = (byte)(D2 >>> 24);

        out[outOffset++] = (byte)(D3       );
        out[outOffset++] = (byte)(D3 >>>  8);
        out[outOffset++] = (byte)(D3 >>> 16);
        out[outOffset  ] = (byte)(D3 >>> 24);
    }


    private static int[] E(int in, int key1, int key2)
    {
        int M = in + key1;                     // (2)
        int R = (in << 13 | in >>> 19) * key2; // (3)
        int i = M & 0x1FF;                     // (4)
        int L = S[i];                          // (5)
        R = R << 5 | R >>> 27;                 // (6)
        int r = R & 0x1F;                      // (7)
        M = M << r | M >>> (32-r);             // (8)
        L ^= R;                                // (9)
        R = R << 5 | R >>> 27;                 // (10)
        L ^= R;                                // (11)
        r = R & 0x1F;                          // (12)
        L = L << r | L >>> (32-r);             // (13)
        return new int[] { L, M, R };          // (14)
    }
}