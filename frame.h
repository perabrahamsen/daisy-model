// frame.h

#ifndef FRAME_H
#define FRAME_H

struct Column;

class Frame
{
  // Backtracking.
private:
  const Frame *const parent;

  // Lookup.
public:
  virtual const Column* column () const;
  virtual bool match_column (const Column&) const;

  // Create and Destroy.
public:
  Frame (const Frame*);
  virtual ~Frame ();
};

#endif FRAME_H
