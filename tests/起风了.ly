\version "2.25.33"
\language "english"

\header {
  title = "起风了"
  arranger = "Arranged by Lex"
  tagline = ""
}

global = {
  \key f \major
  \time 4/4
  \tempo 4 = 75
}

right = \relative c' {
  \global
  \clef treble
   e16 \mp f g a~ a c, c' a~ a2 |
   e16 f g a~ a c, c' a g a f g e16 f c8 |
   e16 f g a~ a c, c' a~ a2 |
   e16 f g a~ a c, c' a g a f g e16 f c8 |
   
   \repeat volta 2 {
     e'16 \f f g a~ a c, c' a~ a2 |
     e16 f g a~ a c, c' a g a f g e16 f c8 |
     e16 f g a~ a c, c' a~ a2 |
     d16\> a g d a d, g d' d,2\! \p |
     
     % Intro done
     \sectionLabel "Verse"
     g8. \( f16 g8. f16 g8 a c a |
     g8. f16 g8. f16 g a g f c4 \) |
     g'8. \( f16 g8. f16 g8 a c a |
     g8. a16 g8 f g2 \) |
     g8. \( f16 g8. f16 g8 a c a |
     g8. a16 g8 f d4 \) a'16 \( g f g |
     f4 \) a16 \( g f g f8. \) c16 a'16 \( g f g |
     f2 \) 
     
     % Pre Chorus
     \sectionLabel "Pre-Chorus"
     f8 \< g a f \! |
     <f d'>8 \mf c'16 d16~  d8. f,16  e'8 d16 e~ e4 |
     <a, e'>8 d16 e~ e8 a, <a f'>16 g' f e d8 c |
     <f, d'>8 c'16 d~ d c d c <g d'>8 c16 g~ g c8. |
     a2 f8 g a f |
     <f d'>8 c'16 d16~  d8. f,16  e'8 d16 e~ e4 |
     <a, e'>8 d16 e~ e8 a, <a f'>16 g' f e d8 c |
     d8 a'16 a~ a8 c, d a'16 a~ a c,8. | 
     <a d>2. \<
     
     % Chorus 
     \sectionLabel "Chorus"
     <f' f'>8 \! \f <g g'>8 |
     a8 d16 c~ c8 d16 c~ c8 d16 c~ c g8. |
     a8 d16 c~ c8 d16 c~ c8 d16 c~ c a8. |
     g8 f16 d~ d f8 d16 g8 f16 d~ d f8. |
     a4~ a16 bf a8 g4 <f f'>8  <g g'>8 |
     a8 d16 c~ c8 d16 c~ c8 d16 c~ c8. g16 |
     a8 d16 c~ c8 d16 c~ c8 d16 c~ c a8. |
     g8 f16 d~ d a'8. g8 f16 d~ d f8. |
     
     \alternative {
       \volta 1 {
         f2. \> d16 \! \mp a'8. |
         g8 f16 d~ d a'8. g8 f16 d~ d f8. |
       }
     }
   }
   
    \sectionLabel "Guitar solo"
     f1 \> |
     R1*7 \! \mp |
     d,8. \< \( e16 f g \! a \> bf c d8. \) r4 \! |
     
    \sectionLabel "Vocals solo"
    R1*4 \p |
    \repeat percent 3 { e16 f g a~ a c, c' a~ a2 | }
    
    \sectionLabel "Final Chorus"
    <d, d'>8 \<<d d'>8 <d d'>8 <d d'>8 <d d'>4 \! \ff
    
    <f f'>16 <g g'>8. |
    a8 d16 c~ c8 d16 c~ c8 d16 c~ c g8. |
    a8 d16 c~ c8 d16 c~ c8 d16 c~ c a8. |
    g8 f16 d~ d f8 d16 g8 f16 d~ d f8. |
    a4~ a16 bf a8 g4 <f f'>8  <g g'>8 |
    a8 d16 c~ c8 d16 c~ c8 d16 c~ c8. g16 |
    a8 d16 c~ c8 d16 c~ c8 d16 c~ c a8. |
    g8 f16 d~ d a'8. g8 f16 d~ d f8. |
    
    f2. \> d16 \! \mp a'8. |
    g8 f16 d~ d a'8. g8 f16 d~ d f8.~ |
    \time 2/4 f2\fermata |
    
    \sectionLabel "Ending"
    \time 4/4
    e16 f g a~ a c, c' a~ a2 |
    e16 f g a~ a c, c' a g a f g e16 f c8 |
    e16 f g a~ a c, c' a~ a2 |
    e16 f g a~ a c, c' a g a f g e16 f c8 |
    e16 f g a~ a c, c' a~ a2 |
    e16 f g a~ a c, c' a g a f g e16 f c8 |
    e16 \p f g a~ a c, c' a~ a2 |
    e16 f g a~ a c, c' a g a f g e16 f d8 |
    c1 |
       
}

left = \relative c {
  \global
  \clef bass
  bf8 f' a4 c,8 e g4 |
  a,8 e' g4 d8 f a4 |
  bf,8 f' a4 c,8 e g4 |
  a,8 e' g4 d8 f a4 |
  
  \repeat volta 2 {
    bf,8 f' a4 c,8 e g4 |
    a,8 e' g4 d8 f a4 |
    bf,8 f' a4 c,8 e g4 |
    <a d fs>2 <d,, d'>2 |
    
    % Verse
    <f' c'>1 |
    <e c'>1 |
    <ef c'>1 |
    <bf f'>1 |
    <bf f'>2  <c g'>2 |
    <d a'>1 |
    <bf f'>2 <c g'>2 |
    f,8 c' f g 
    
    % Pre Chorus 
    a2 |
    bf,8 f' bf f c g' c g |
    a,8 e' a e d a' d a |
    bf,8 f' bf f c g' c g |
    f8 c' f c <ef, c'>4 <f ef'>4 |
    bf,8 f' bf f c g' c g |
    a,8 e' cs' e, d a' d a |
    bf,8 f' bf f c g' c g |
    <a d>8 <a d>8 <a d>8 <a d>8 <a d>2 |
    
    % Chorus
    bf,8 f' bf f c g' c g |
    a,8 e' a e d a' d a |
    bf,8 f' bf f c g' c g |
    f,8 c' f c a e' cs' e, |
    bf8 f' bf f c g' c g |
    a,8 e' a e d a' d a |
    bf,8 f' bf4 c,8 g' c4 |
      
    \alternative {
       \volta 1 {
         d,8 a' d a f'2 |
         <bf,, f'>2 <c g'>2 |
       }
     }
  }
  
  % Guitar solo
  <d, d'>8 <d d'> <d d'> <d d'> <d d'> <d d'> <d d'> <d d'> |
  <bf' f' bf>4 <bf f' bf>4 <c g' c>4 <c g' c>4 |
  <a e' a>4 <a e' a>4 <d a' d>4 <d a' d>4 |
  <bf f' bf>4 <bf f' bf>4 <c g' c>4 <c g' c>4 |
  <f, c' f>4 <f c' f>4 <a e' a>4 <a e' a>4 |
  <bf f' bf>4 <bf f' bf>4 <c g' c>4 <c g' c>4 |
  <a e' a>4 <a e' a>4 <d a' d>4 <d a' d>4 |
  <bf f' bf>4 <bf f' bf>4 <c g' c>4 <c g' c>4 |
  <d a' d>8. <d a' d>8 <d a' d>8 <d a' d>8 <d d'>4. r16|
  
  % Vocals solo
  <bf  bf'>2 <c c'>2 |
  <a a'>2 <d d'>2 |
  <bf  bf'>2 <c c'>2 |
  f,8 c' f4 <a, a'>2 |
  <bf bf'>2 <c c'>2 |
  <a a'>2 <d d'>2 |
  bf8 f' a4 c,8 e g4 |
  <d d'>8 <d d'>8 <d d'>8 <d d'>8 <d d'>2 |
  
  % FINAL CHORUS
  bf8 f' bf f c g' c g |
  a,8 e' a e d a' d a |
  bf,8 f' bf f c g' c g |
  f,8 c' f c a e' cs' e, |
  bf8 f' bf f c g' c g |
  a,8 e' a e d a' d a |
  bf,8 f' bf4 c,8 g' c4 |
  
  % Final bridge
  d,8 a' d a f'2 |
  <bf,, f'>2 <c g'>2 |
  r2 |
  
  % Ending
  <bf f' a>4 <bf f' a>4 <c e g>4 <c e g>4 |
  <a e' g>4 <a e' g>4 <d f a>4 <d f a>4 |
  <bf f' a>4 <bf f' a>4 <c e g>4 <c e g>4 |
  <a e' g>4 <a e' g>4 <d f a>4 <d f a>4 |
  <bf f' a>4 <bf f' a>4 <c e g>4 <c e g>4 |
  <a e' g>4 <a e' g>4 <d f a>4 <d f a>4 |
  bf8 f' a4 c,8 e g4 |
  a,8 e' g4 d8 f a4~ |
  a1 |

}

\score {
  <<
  \new PianoStaff \with {
    instrumentName = "Piano"
  } <<
    \new Staff = "right" \right
    \new Staff = "left" \left
  >>
  
  >>
  \midi { }
  \layout { }
}