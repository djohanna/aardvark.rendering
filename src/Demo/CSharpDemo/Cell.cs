using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Aardvark.Base;

namespace CSharpDemo
{
    /// <summary>
    /// A 2^Exponent sized cube positioned at (X,Y,Z) * 2^Exponent.
    /// </summary>
    public struct Cell : IEquatable<Cell>
    {
        public readonly long X;
        public readonly long Y;
        public readonly long Z;
        public readonly int Exponent;

        /// <summary>
        /// Cell with min corner at (x,y,z)*2^exponent and dimension 2^exponent.
        /// </summary>
        public Cell(long x, long y, long z, int exponent)
        {
            X = x; Y = y; Z = z; Exponent = exponent;
            BoundingBox = ComputeBoundingBox(X, Y, Z, Exponent);
        }

        /// <summary>
        /// Cell with min corner at index*2^exponent and dimension 2^exponent.
        /// </summary>
        public Cell(V3l index, int exponent) : this(index.X, index.Y, index.Z, exponent) { }

        /// <summary>
        /// Special cell, which is centered at origin, with dimension 2^exponent.
        /// </summary>
        public Cell(int exponent) : this(long.MaxValue, long.MaxValue, long.MaxValue, exponent) { }

        /// <summary>
        /// Smallest cell that contains given box.
        /// </summary>
        public Cell(Box3d box)
        {
            if (!box.IsValid || box.Min == box.Max) throw new InvalidOperationException();

            // case 1: contains origin
            if (box.Min.X < 0.0 && box.Max.X > 0.0 ||
                box.Min.Y < 0.0 && box.Max.Y > 0.0 ||
                box.Min.Z < 0.0 && box.Max.Z > 0.0)
            {
                X = Y = Z = long.MaxValue;
                Exponent = Math.Max(box.Min.NormMax, box.Max.NormMax).Log2Int() + 1;
            }
            else // case 2: doesn't contain origin
            {
                Exponent = box.Size.NormMax.Log2Int();
                var s = Math.Pow(2.0, Exponent);
                var a = (box.Min / s).Floor * s;
                while (a.X + s < box.Max.X || a.Y + s < box.Max.Y || a.Z + s < box.Max.Z)
                {
                    s *= 2.0; Exponent++;
                    a = (box.Min / s).Floor * s;
                }
                X = (long)Math.Floor(a.X / s);
                Y = (long)Math.Floor(a.Y / s);
                Z = (long)Math.Floor(a.Z / s);
            }

            BoundingBox = ComputeBoundingBox(X, Y, Z, Exponent);
        }

        /// <summary>
        /// Gets whether this cell is a special cell centered at origin.
        /// </summary>
        public bool IsCenteredAtOrigin => X == long.MaxValue && Y == long.MaxValue && Z == long.MaxValue;

        /// <summary>
        /// Returns true if the cell completely contains the other cell.
        /// </summary>
        public bool Contains(Cell other)
        {
            return BoundingBox.Contains(other.BoundingBox);

            // !!! below: ~8x speedup, but is it correct? ;-)
            // !!! does not handle special cells centered at origin
            //if (other.Exponent > Exponent) return false;
            //if (other.Exponent == Exponent) return other.X == X && other.Y == Y && other.Z == Z;
            //var d = 1 << (Exponent - other.Exponent);
            //var x0 = X * d; var y0 = Y * d; var z0 = Z * d;
            //var x1 = x0 + d; var y1 = y0 + d; var z1 = z0 + d;
            //return other.X >= x0 && other.X < x1 && other.Y >= y0 && other.Y < y1 && other.Z >= z0 && other.Z < z1;
        }

        /// <summary>
        /// Returns true if two cells intersect each other.
        /// </summary>
        public bool Intersects(Cell other)
        {
            return BoundingBox.Intersects(other.BoundingBox);
        }

        /// <summary>
        /// Gets indices of the 8 subcells.
        /// </summary>
        public Cell[] Children
        {
            get
            {
                var x0 = X << 1; var y0 = Y << 1; var z0 = Z << 1;
                var x1 = x0 + 1; var y1 = y0 + 1; var z1 = z0 + 1;
                var e = Exponent - 1;
                return new[]
                {
                    new Cell(x0, y0, z0, e),
                    new Cell(x1, y0, z0, e),
                    new Cell(x0, y1, z0, e),
                    new Cell(x1, y1, z0, e),
                    new Cell(x0, y0, z1, e),
                    new Cell(x1, y0, z1, e),
                    new Cell(x0, y1, z1, e),
                    new Cell(x1, y1, z1, e),
                };
            }
        }

        /// <summary>
        /// Gets cell's bounds.
        /// </summary>
        public Box3d BoundingBox { get; }

        /// <summary>
        /// Octant 0-7.
        /// 0th, 1st and 2nd bit encodes x-, y-, z-axis, respectively.
        /// E.g. 0 is octant at origin, 7 is octant oposite from origin.
        /// </summary>
        public Cell GetOctant(int i)
        {
            if (i < 0 || i > 7) throw new IndexOutOfRangeException();
            return new Cell(
                (X << 1) + ((i & 1) == 0 ? 0 : 1),
                (Y << 1) + ((i & 2) == 0 ? 0 : 1),
                (Z << 1) + ((i & 4) == 0 ? 0 : 1),
                Exponent - 1
                );
        }

        #region equality and hashing

        public bool Equals(Cell other) => this == other;
        public static bool operator ==(Cell a, Cell b) => a.X == b.X && a.Y == b.Y && a.Z == b.Z && a.Exponent == b.Exponent;
        public static bool operator !=(Cell a, Cell b) => !(a == b);
        public override bool Equals(object obj) => obj is Cell && this == (Cell)obj;
        public override int GetHashCode() => HashCode.GetCombined(X, Y, Z, Exponent);

        #endregion


        public override string ToString()
        {
            return $"[{X}, {Y}, {Z}, {Exponent}]";
        }

        private static Box3d ComputeBoundingBox(long x, long y, long z, int e)
        {
            var d = Math.Pow(2.0, e);
            var isCenteredAtOrigin = x == long.MaxValue && y == long.MaxValue && z == long.MaxValue;
            var min = isCenteredAtOrigin ? new V3d(-0.5 * d) : new V3d(x * d, y * d, z * d);
            return Box3d.FromMinAndSize(min, new V3d(d, d, d));
        }
    }
}
